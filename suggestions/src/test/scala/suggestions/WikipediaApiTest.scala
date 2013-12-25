package suggestions

import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import gui._
import rx.lang.scala._

@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => {
        assert(false, s"stream error $t")
      },
      () => {
        completed = true
      })
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should recover") {
    val notvalid = Observable(0 to 20).map(x => if (x == 20) throw new Error else x)
    val valid = notvalid.recovered
    var count = 0
    var completed = false

    val sub = valid.subscribe(
      t => {
        count += 1
      },
      e => {
        assert(false, s"should never have error: $e")
      },
      () => {
        completed = true
      })
    assert(completed && count == 21, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should timeout") {
    val valid = Observable(1, 2, 3).zip(Observable.interval(700 millis)).timedOut(1)
    var count = 0
    var completed = false

    val sub = valid.subscribe(
      t => {
        count += 1
      },
      e => {
        println(s"error: $e")
        assert(false, s"stream error $e")
      },
      () => {
        println("ended")
        completed = true
      })
    assert(completed && count == 1, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(0 to 10)
    val remoteComputation = (n: Int) => Observable(n, n)
    val responses = requests concatRecovered remoteComputation

    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => acc
      }
    }

    var total = -1
    val sub = sum.subscribe(s => total = s)

    assert(total == 110, s"Sum: $total")
  }

  test("WikipediaApi should concat when error") {
    val requests = Observable(0 to 10)
    val remoteComputation = (n: Int) => if (n != 5) Observable(n, n) else throw new Error("2")
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => acc
      }
    }
    var total = -1
    val sub = sum.subscribe(s => total = s)

    assert(total == 95, s"Sum: $total")
  }
}