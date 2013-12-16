/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /**
   * Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /**
   * Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /**
   * Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

object NextIndex {
  var i = 0;
  def apply() = {
    i += 1
    i
  }
}

object Log {
  var enabled = false
  def apply(s: String) = {
    if (enabled) println(s)
  }
}

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), s"root${NextIndex()}")

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC => {
      val tempRoot = createRoot
      root ! CopyTo(tempRoot)
      context.become(garbageCollecting(tempRoot))
    }
    case x => root ! x
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => {
      root = newRoot
      
      context.become(normal)
      
      pendingQueue.foreach(root ! _)
      pendingQueue = Queue.empty
    }
    case GC => ()
    case x: Operation => pendingQueue = pendingQueue.enqueue(x)
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean = false) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def hasLeft = subtrees.contains(Left)
  def hasRight = subtrees.contains(Right)
  def left = subtrees(Left)
  def right = subtrees(Right)
  def isEmpty = subtrees.isEmpty

  def pad(s: String, to: Int) =
    s.padTo(to, " ").foldRight("")((next, acc) => next + acc)
    
  override def toString(): String = 
    pad(s"[$elem, $removed, ${self.path.toString.replace(self.path.root.toString, "")}]", 50)

  def pid(id: Int) = pad(s"[$id]", 6)
    
  /**
   * INSERT
   */
  def insert(requester: ActorRef, id: Int, toInsert: Int) = {
    def addNode(pos: Position): Unit = {
      // Log(s"${pid(id)} ADD [$toInsert] to $this [$pos] ")
      subtrees += (pos -> context.actorOf(BinaryTreeNode.props(toInsert), s"$toInsert"))
      requester ! OperationFinished(id)
    }

    def addNodeRight = addNode(Right)
    def addNodeLeft = addNode(Left)

    def insertToPosition(pos: Position) =
      if (subtrees.contains(pos)) subtrees(pos) ! Insert(requester, id, toInsert)
      else addNode(pos)

    def insertLeft = insertToPosition(Left)
    def insertRight = insertToPosition(Right)

    Log(s"${pid(id)} $this INSERT [$toInsert]")

    // implementation
    if (toInsert < elem) insertLeft
    else if (toInsert > elem) insertRight
    else {
      removed = false
      requester ! OperationFinished(id)
    }
  }

  /**
   * CONTAINS
   */
  def contains(requester: ActorRef, id: Int, toFind: Int) = {
    Log(s"${pid(id)} $this CONTAINS [$toFind]")
    if (elem == toFind && !removed) requester ! ContainsResult(id, true)
    else {
      if (hasLeft && toFind < elem) left ! Contains(requester, id, toFind)
      else if (hasRight && toFind > elem) right ! Contains(requester, id, toFind)
      else requester ! ContainsResult(id, false)
    }
  }
  
  /**
   * REMOVE
   */
  def remove(requester: ActorRef, id: Int, toRemove: Int) = {
    Log(s"${pid(id)} $this REMOVE [$toRemove]")
    if (elem == toRemove && !removed) {
      removed = true
      requester ! OperationFinished(id)
    }
    else {
      if (hasLeft && toRemove < elem) left ! Remove(requester, id, toRemove)
      else if (hasRight && toRemove > elem) right ! Remove(requester, id, toRemove)
      else requester ! OperationFinished(id)
    }
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, toInsert) => insert(requester, id, toInsert)
    case Remove(requester, id, toRemove) => remove(requester, id, toRemove)
    case Contains(requester, id, toFind) => contains(requester, id, toFind)
    case CopyTo(newRoot) => {
      if (!removed) {
        Log (s"${pid(-1)} $this COPY")
        newRoot ! Insert(self, -1, elem)
      }
      
      if (hasLeft) left ! CopyTo(newRoot)
      if (hasRight) right ! CopyTo(newRoot)
      
      if (!removed || hasLeft || hasRight) context.become(copying(subtrees.values.toSet, removed))
	  else context.parent ! CopyFinished
    	    
    }
  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    def me = s"[$expected, $insertConfirmed]"
    
    _ match {
      case OperationFinished(_) => {
        Log(s"${pid(-1)} $this COPY INSERT Finished $me")
        if (expected.isEmpty) {
          context.parent ! CopyFinished
          context.stop(self)
        }
        else context.become(copying(expected, true))
      }
      case CopyFinished => {
        Log(s"${pid(-1)} $this CHILD COPY $sender Finished $me")
        val newExpected = expected - sender

        if (newExpected.isEmpty && insertConfirmed) {
          context.parent ! CopyFinished
          context.stop(self)
        }
        else context.become(copying(newExpected, insertConfirmed))
      }
    }
  }

}
