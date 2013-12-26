package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var persistenceAcks = Set.empty[Long]
  var replAcksOutstanding = Map.empty[Long, Set[ActorRef]]
  var operationSender = Map.empty[Long, ActorRef]
  
  override def preStart(): Unit = {
    arbiter ! Join
  }
  
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5) {
     case _: PersistenceException => SupervisorStrategy.Restart
  }

  val persister = context.actorOf(persistenceProps, "persister")

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  def get(key: String, id: Long) = {
    sender ! GetResult(key, kv.get(key), id)
  }

  def persist(key: String, value: Option[String], seq: Long): Unit = {
    persistenceAcks += seq

    persister ! Persist(key, value, seq)

    context.system.scheduler.scheduleOnce(100 millis) {
      if (persistenceAcks.contains(seq)) persist(key, value, seq)
    }
  }

  def canSendAck(id: Long) = {
    !persistenceAcks.contains(id) && !replAcksOutstanding.contains(id)
  }

  def replicate(key: String, value: Option[String], id: Long) = {
    replicators foreach { repl =>
      replAcksOutstanding.get(id) match {
        case Some(repls) => replAcksOutstanding += (id -> (repls + repl))
        case None => replAcksOutstanding += (id -> Set(repl))
      }
      repl ! Replicate(key, value, id)
    }
  }

  def persistAndReplicate(key: String, value: Option[String], id: Long) = {
    operationSender += (id -> sender)
    persist(key, value, id)
    replicate(key, value, id)

    context.system.scheduler.scheduleOnce(1 second) {
      if (!canSendAck(id)) {
        sendOperationReply(id, OperationFailed(id))
        persistenceAcks -= id
        replAcksOutstanding -= id
      }
    }
  }

  def sendOperationReply(id: Long, response: OperationReply) {
    operationSender(id) ! response
    operationSender -= id
  }

  def handleNewReplica(sender: ActorRef)(added: ActorRef) = {
    val repl = context.actorOf(Replicator.props(added))
    replicators += repl
    secondaries += (added -> repl)
    var _id = 0L;
    kv foreach { entry =>
      val (k, v) = entry
      operationSender += _id -> sender
      repl ! Replicate(k, Some(v), _id)
      _id += 1
    }
  }

  def handleRemovedReplicas(removed: ActorRef) = {
    val replicator = secondaries(removed)
    replicator ! PoisonPill
    context.stop(removed)
    replicators -= replicator
    secondaries -= removed

    replAcksOutstanding.map { entry =>
      val (id, repls) = entry
      val newOutstanding = repls - replicator
      
      if (newOutstanding.isEmpty) {
        println("empty")
        sendOperationReply(id, OperationAck(id))
      }
      
      id -> newOutstanding
    }
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Replicas(reps) => {
      val newReplicas = reps - self -- secondaries.keys
      newReplicas foreach handleNewReplica(sender)

      val removedReplicas = secondaries.keySet -- reps
      removedReplicas foreach handleRemovedReplicas
    }
    case Get(key, id) => get(key, id)
    case Insert(key, value, id) => {
      kv += (key -> value)
      persistAndReplicate(key, Some(value), id)
    }
    case Remove(key, id) => {
      kv -= key
      persistAndReplicate(key, None, id)
    }
    case Persisted(key, id) => {
      persistenceAcks -= id

      if (canSendAck(id)) sendOperationReply(id, OperationAck(id))
    }
    case Replicated(key, id) => {
      if (replAcksOutstanding.contains(id)) {
        replAcksOutstanding += (id -> (replAcksOutstanding(id) - sender))
        if (replAcksOutstanding(id).isEmpty) replAcksOutstanding -= id
      }

      if (canSendAck(id)) sendOperationReply(id, OperationAck(id))
    }
  }

  var currSeq: Long = 0

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => get(key, id)
    case Snapshot(key, value, seq) => {
      if (seq == currSeq) {
        value match {
          case Some(value) => kv += (key -> value)
          case None => kv -= key
        }
        operationSender += (seq -> sender)
        persist(key, value, seq)

        currSeq += 1
      } else if (seq < currSeq) {
        sender ! SnapshotAck(key, seq)
      }
    }
    case Persisted(key, seq) => {
      operationSender(seq) ! SnapshotAck(key, seq)
      persistenceAcks -= seq
    }
  }

}
