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

  override def preStart(): Unit = {
    arbiter ! Join
    context.actorOf(persistenceProps, "persister")
  }

  def persister = context.child("persister").get

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  def get(key: String, id: Long) = {
    sender ! GetResult(key, kv.get(key), id)
  }

  def persist(key: String, value: Option[String], seq: Long): Unit = {
    val event = Persist(key, value, seq)
    
    persister ! event

    context.system.scheduler.scheduleOnce(100 millis) {
      if (persistenceAcks.contains(seq)) persist(key, value, seq)
    }
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Get(key, id) => get(key, id)
    case Insert(key, value, id) => {
      kv += (key -> value)
      sender ! OperationAck(id)
    }
    case Remove(key, id) => {
      kv -= key
      sender ! OperationAck(id)
    }
    case _ =>
  }

  var currSeq: Long = 0

  var persistenceAcks = Map.empty[Long, ActorRef]

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => get(key, id)
    case Snapshot(key, value, seq) => {
      if (seq == currSeq) {
        value match {
          case Some(value) => kv += (key -> value)
          case None => kv -= key
        }
        persistenceAcks += (seq -> sender)
        
        persist(key, value, seq)
        
        currSeq += 1
      }
      else if (seq < currSeq) {
        sender ! SnapshotAck(key, seq)
      }
    }
    case Persisted(key, seq) => {
      persistenceAcks(seq) ! SnapshotAck(key, seq)
      persistenceAcks -= seq
    }
  }

}
