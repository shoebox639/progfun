package kvstore

import scala.concurrent.duration.DurationInt

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case Replicate(key, value, id) => {
      def replicate(key: String, value: Option[String], seq: Long): Unit = {
        replica ! Snapshot(key, value, seq)

        context.system.scheduler.scheduleOnce(100 millis) {
          if (acks.contains(seq)) replicate(key, value, seq)
        }
      }

      val seq = nextSeq
      acks += (seq -> (sender, Replicate(key, value, id)))
      replicate(key, value, seq)
    }
    case SnapshotAck(key, seq) => {
      acks.get(seq) map {
        case (sndr, repl) =>
          sndr ! Replicated(key, repl.id)
          acks -= seq
      }
    }
  }

}
