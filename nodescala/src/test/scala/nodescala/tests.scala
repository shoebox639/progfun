package nodescala

import scala.async.Async.async
import scala.async.Async.await
import scala.collection._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import NodeScala._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Empty List returns Future of empty last") {
    val list = List[Future[Int]]()

    assert(Await.result(Future.all(list), 1 second) == Nil)
  }

  test("One item returns Future of it") {
    val list = List[Future[Int]](Future { 1 })

    assert(Await.result(Future.all(list), 1 second) == List(1))
  }

  test("Two item returns Future of both") {
    val list = List[Future[Int]](Future { 1 }, Future { 2 })

    assert(Await.result(Future.all(list), 1 second) == List(1, 2))
  }

  test("One Exception and one item returns Future of both") {
    val list = List[Future[Int]](Future { 1 }, Future { throw new Error })

    try {
      Await.result(Future.all(list), 0 nanos)
      assert(false)
    } catch {
      case t: Any => assert(true)
    }
  }

  test("Any with one item returns success/failure of that item") {
    lazy val b1 = blocking {
      Thread.sleep(1000)
      1
    }
    val list = List[Future[Int]](Future(b1), Future(2))

    assert(Await.result(Future.any(list), 1 nanos) == 2)
  }

  test("Any with one item throwing exception first, returns exception") {
    lazy val b1 = blocking {
      Thread.sleep(1000)
      1
    }
    val list = List[Future[Int]](Future(b1), Future(throw new NoSuchElementException))

    try {
      Await.result(Future.any(list), 1 nanos)
      fail
    } catch {
      case t: NoSuchElementException => //ok
    }
  }

  test("Delay should throw if not finished") {
    try {
      Await.result(Future.delay(1 second), 1 nanos)
      fail
    }
    catch {
      case t: TimeoutException => //ok
    }
  }

  test("Now fails when not waiting long enough") {
    lazy val b1 = blocking {
      Thread.sleep(1000)
      1
    }

    try {
      Future(b1).now
      fail
    } catch {
      case t: NoSuchElementException => //ok
    }
  }

  test("Now succeeds when waiting long enough") {
    try {
      Future(1).now
      fail
    } catch {
      case t: NoSuchElementException => //ok
    }
  }

  test("ContinueWith doesn't act on unfinished futures") {
    lazy val b1 = blocking {
      Thread.sleep(1000)
      1
    }

    val future = Future[Int](b1) continueWith { _ => 3 }

    try {
      Await.result(future, 1 nanos)
    } catch {
      case e: TimeoutException => // ok
    }
  }

  test("ContinueWith maps failure correctly") {
    val future = Future[Int](throw new NoSuchElementException) continueWith {
      _.value match {
        case Some(Success(x)) => "success"
        case Some(Failure(t)) => "failure"
        case None => "not finished"
      }
    }

    assert(Await.result(future, 1 nanos) == "failure")
  }

  test("ContinueWith maps success correctly") {
    val future = Future[Int](1) continueWith {
      _.value match {
        case Some(Success(x)) => "success"
        case Some(Failure(t)) => "failure"
        case None => "not finished"
      }
    }

    assert(Await.result(future, 1 nanos) == "success")
  }

  test("Continue doesn't act when not complete") {
    lazy val b1 = blocking {
      Thread.sleep(1000)
      1
    }
    val future = Future(b1) continue {
      case Success(x) => "success"
      case Failure(t) => "failure"
    }

    try {
      Await.result(future, 1 nanos)
    } catch {
      case e: TimeoutException => // ok
    }
  }

  test("Continue maps success correctly") {
    val future = Future(1) continue {
      case Success(x) => "success"
      case Failure(t) => "failure"
    }

    assert(Await.result(future, 1 nanos) == "success")
  }

  test("Continue maps failures correctly") {
    val future = Future(1 / 0) continue {
      case Success(x) => "success"
      case Failure(t) => "failure"
    }

    assert(Await.result(future, 1 nanos) == "failure")
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  test("cancellation test") {
    //    val working = Future.run() { ct =>
    //      Future {
    //        while (ct.nonCancelled) {
    //          println("working")
    //        }
    //        println("done")
    //      }
    //    }
    //    Future.delay(1 seconds) onSuccess {
    //      case _ => working.unsubscribe()
    //    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




