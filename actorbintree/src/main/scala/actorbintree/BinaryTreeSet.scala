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

class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case x => root ! x
  }

  // optional
  /**
   * Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

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
  
  override def toString(): String = s"[$elem, $removed]"

  /**
   * INSERT
   */
  def insert(requester: ActorRef, id: Int, toInsert: Int) = {
    def addNode(pos: Position): Unit = {
      println(s"[$id] ADD [$toInsert] to $this [$pos] ")
      subtrees += (pos -> context.actorOf(BinaryTreeNode.props(toInsert)))
      requester ! OperationFinished(id)
    }

    def addNodeRight = addNode(Right)
    def addNodeLeft = addNode(Left)

    def insertToPosition(pos: Position) =
      if (subtrees.contains(pos)) subtrees(pos) ! Insert(requester, id, toInsert)
      else addNode(pos)

    def insertLeft = insertToPosition(Left)
    def insertRight = insertToPosition(Right)

    println(s"[$id] INSERT [$toInsert] into $this")

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
    println(s"[$id] CONTAINS [$toFind] at $this")
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
    println(s"[$id] REMOVE [$toRemove] at $this")
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
    case _ => ???
  }

  // optional
  /**
   * `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}
