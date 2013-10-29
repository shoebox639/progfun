package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val a = new Tweet("a", "a body", 20);
    var b = new Tweet("b", "b body", 20);
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set2 = set1.incl(a)
    val set3 = set2.incl(b)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("union: a U b U c => [a, b, c]") {
    new TestSets {
      new Empty().incl(a).union(new Empty().incl(b)).union(new Empty().incl(c)) foreach println
    }
  }
  
  test("mostRetweets:set of one returns it") {
    new TestSets {
      assert(new Empty().incl(a).mostRetweeted == a)
    }
  }
  
  test("mostRetweets:set returns one with most tweets") {
    new TestSets {
      assert(new Empty().incl(a).incl(c).incl(d).mostRetweeted == a)
      assert(new Empty().incl(c).incl(a).incl(d).mostRetweeted == a)
      assert(new Empty().incl(c).incl(d).incl(a).mostRetweeted == a)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  
  test("tredning test: everything descending") {
    def listInDescendingOrder(list: TweetList): Boolean = {
      listInDescAcc(list.tail.tail, false)
    }
    def listInDescAcc(list: TweetList, acc: Boolean): Boolean = {
      if (list == Nil || list.tail == Nil) acc
      else {
        val first = list.head
        val next = list.tail.head
        listInDescAcc(list.tail.tail, first.retweets >= next.retweets || acc)
      }
    }
    
    assert(listInDescendingOrder(GoogleVsApple.trending))
  }
  
}
