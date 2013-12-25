/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.{ Props, ActorRef, ActorSystem }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import scala.concurrent.duration._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import BinaryTreeNode.{CopyTo, CopyFinished}

@RunWith(classOf[JUnitRunner])
class BinaryTreeSuite(_system: ActorSystem) extends TestKit(_system) with FunSuite with ShouldMatchers with BeforeAndAfterAll with ImplicitSender {

  def this() = this(ActorSystem("PostponeSpec"))

  override def afterAll: Unit = system.shutdown()

  import actorbintree.BinaryTreeSet._

  def receiveN(requester: TestProbe, ops: Seq[Any], expectedReplies: Seq[Any]): Unit =
    within(5.seconds) {
      val repliesUnsorted = for (i <- 1 to ops.size) yield try {
        requester.expectMsgType[OperationReply]
      } catch {
        case ex: Throwable if ops.size > 10 => fail(s"failure to receive confirmation $i/${ops.size}", ex)
        case ex: Throwable => fail(s"failure to receive confirmation $i/${ops.size}\nRequests:" + ops.mkString("\n    ", "\n     ", ""), ex)
      }
      val replies = repliesUnsorted.sortBy(_.id)
      if (replies != expectedReplies) {
        val pairs = (replies zip expectedReplies).zipWithIndex filter (x => x._1._1 != x._1._2)
        fail("unexpected replies:" + pairs.map(x => s"at index ${x._2}: got ${x._1._1}, expected ${x._1._2}").mkString("\n    ", "\n    ", ""))
      }
    }

  def verify(probe: TestProbe, ops: Seq[Any], expected: Seq[Any]): Unit = {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    ops foreach { op =>
      topNode ! op
    }

    receiveN(probe, ops, expected)
  }

  def t(testName: String)(block: Unit) {
    //println(s"--STARTING TEST: $testName --")
    test(testName)(block)
  }

  t("insert sends operation finished") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Insert(testActor, id = 1, 1)
    expectMsg(OperationFinished(1))
  }

  t("remove sends operation finished") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Remove(testActor, id = 1, 1)
    expectMsg(OperationFinished(1))
  }

  t("contains returns false on empty root") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))
  }

  t("contains responds false for any for empty") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Contains(testActor, id = 3, 0)

    expectMsg(ContainsResult(3, false))
  }

  t("contains responds false when element not present") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Insert(testActor, id = 2, 5)
    topNode ! Contains(testActor, id = 3, 1)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, false))
  }

  t("proper inserts right and lookups") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Insert(testActor, id = 2, 1)
    topNode ! Contains(testActor, id = 3, 1)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))
  }

  t("proper inserts left and lookups") {
    val topNode = system.actorOf(Props[BinaryTreeSet])
    val v = -1

    topNode ! Insert(testActor, id = 2, v)
    topNode ! Contains(testActor, id = 3, v)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))
  }

  t("proper right then left left and lookups") {
    val topNode = system.actorOf(Props[BinaryTreeSet])
    val v1 = 4
    val v2 = 2

    topNode ! Insert(testActor, id = 2, v1)
    topNode ! Insert(testActor, id = 3, v2)
    topNode ! Contains(testActor, id = 4, v1)
    topNode ! Contains(testActor, id = 5, v2)

    expectMsg(OperationFinished(2))
    expectMsg(OperationFinished(3))
    expectMsg(ContainsResult(4, true))
    expectMsg(ContainsResult(5, true))
  }

  def insert(requester: ActorRef, value: Int)(id: Int) = Insert(requester, id, value)
  def contains(requester: ActorRef, value: Int)(id: Int) = Contains(requester, id, value)
  def remove(requester: ActorRef, value: Int)(id: Int) = Remove(requester, id, value)
  def operationFinished = { id: Int => OperationFinished(id) }
  def containsResult(value: Boolean)(id: Int) = ContainsResult(id, value)
  def gc = { i: Int => GC }

  def createOperations(opFuncs: List[Int => Any]): List[Any] = {
    val opsWithIndex = opFuncs.zip(1 to opFuncs.length)

    for ((op, index) <- opsWithIndex) yield op(index)
  }

  t("insert, remove, then insert it again, we should find it") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val v1 = 4

    val ops = createOperations(List(
      insert(requesterRef, v1),
      contains(requesterRef, v1),
      remove(requesterRef, v1),
      contains(requesterRef, v1),
      insert(requesterRef, v1),
      contains(requesterRef, v1)))

    val expectedReplies = createOperations(List(
      operationFinished,
      containsResult(true),
      operationFinished,
      containsResult(false),
      operationFinished,
      containsResult(true)))

    verify(requester, ops, expectedReplies)
  }

  t("insert 1-10, copy it, then it should have all the items in first list") {
    val original = system.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), "original")
    val copy = system.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true), "copy")

    original ! Insert(testActor, 1, 3)
    original ! Insert(testActor, 2, 5)
    original ! Insert(testActor, 3, 1)
    original ! Insert(testActor, 4, 2)
    original ! Insert(testActor, 5, 4)
    
    expectMsg(OperationFinished(1))
    expectMsg(OperationFinished(2))
    expectMsg(OperationFinished(3))
    expectMsg(OperationFinished(4))
    expectMsg(OperationFinished(5))

	original ! CopyTo(copy)
	Thread.sleep(2000)
    
    copy ! Contains(testActor, 6, 3)
    copy ! Contains(testActor, 7, 5)
    copy ! Contains(testActor, 8, 1)
    copy ! Contains(testActor, 9, 2)
    copy ! Contains(testActor, 10, 4)
    
    expectMsg(ContainsResult(6, true))
    expectMsg(ContainsResult(7, true))
    expectMsg(ContainsResult(8, true))
    expectMsg(ContainsResult(9, true))
    expectMsg(ContainsResult(10, true))
    
  }

  test("instruction example") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Insert(requesterRef, id = 100, 1),
      Contains(requesterRef, id = 50, 2),
      Remove(requesterRef, id = 10, 1),
      Insert(requesterRef, id = 20, 2),
      Contains(requesterRef, id = 80, 1),
      Contains(requesterRef, id = 70, 2))

    val expectedReplies = List(
      OperationFinished(id = 10),
      OperationFinished(id = 20),
      ContainsResult(id = 50, false),
      ContainsResult(id = 70, true),
      ContainsResult(id = 80, false),
      OperationFinished(id = 100))

    verify(requester, ops, expectedReplies)
  }

    test("behave identically to built-in set (includes GC)") {
      val rnd = new Random()
      def randomOperations(requester: ActorRef, count: Int): Seq[Operation] = {
        def randomElement: Int = rnd.nextInt(100)
        def randomOperation(requester: ActorRef, id: Int): Operation = rnd.nextInt(4) match {
          case 0 => Insert(requester, id, randomElement)
          case 1 => Insert(requester, id, randomElement)
          case 2 => Contains(requester, id, randomElement)
          case 3 => Remove(requester, id, randomElement)
        }
  
        for (seq <- 0 until count) yield randomOperation(requester, seq)
      }
  
      def referenceReplies(operations: Seq[Operation]): Seq[OperationReply] = {
        var referenceSet = Set.empty[Int]
        def replyFor(op: Operation): OperationReply = op match {
          case Insert(_, seq, elem) =>
            referenceSet = referenceSet + elem
            OperationFinished(seq)
          case Remove(_, seq, elem) =>
            referenceSet = referenceSet - elem
            OperationFinished(seq)
          case Contains(_, seq, elem) =>
            ContainsResult(seq, referenceSet(elem))
        }
  
        for (op <- operations) yield replyFor(op)
      }
  
      val requester = TestProbe()
      val topNode = system.actorOf(Props[BinaryTreeSet])
      val count = 1000
  
      val ops = randomOperations(requester.ref, count)
      val expectedReplies = referenceReplies(ops)
  
      ops foreach { op =>
        topNode ! op
        if (rnd.nextDouble() < 0.1) topNode ! GC
      }
      receiveN(requester, ops, expectedReplies)
    }
}