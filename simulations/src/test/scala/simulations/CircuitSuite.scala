package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    for (a <- List(true, false); b <- List(true, false)) {
      in1.setSignal(a);
      in2.setSignal(b);
      run;
      assert(out.getSignal === (a || b))
    }
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    for (a <- List(true, false); b <- List(true, false)) {
      in1.setSignal(a);
      in2.setSignal(b);
      run;
      assert(out.getSignal === (a || b))
    }
  }

  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  test("demux no inputs") {
    val in = new Wire
    val out1 = List(new Wire)
    demux(in, Nil, out1)
    in.setSignal(true)
    run
    assert(out1.indexWhere(_.getSignal) === 0)

    in.setSignal(false)
    run
    assert(out1.indexWhere(_.getSignal) === -1)
  }

  test("demux") {
    val bits = 2
    val max = pow(2, bits).toInt
    val in = new Wire
    val cin = (for (i <- 1 to bits) yield new Wire) toList
    val out = (for (i <- 1 to max) yield new Wire) toList

    demux(in, cin, out)
    in.setSignal(true)

    def loop(i: Int, v: Int): Unit = {
      if (i == 0) {
        for {
          a <- List(true, false)
        } {
          cin(i).setSignal(a)
          val actual = v * 2 + a;

          run;
          val exp = out.indexWhere(_.getSignal);
          println(s"cin: ${p(cin)}, v: $actual, exp: $exp, out: ${p(out)}")

          // assert(exp === actual, actual)
          // else assert(out.indexWhere(_.getSignal) === -1)
        }
      } else {
        for (a <- List(true, false)) {
          cin(i).setSignal(a)
          loop(i - 1, v * 2 + a)
        }
      }
    }
    loop(bits - 1,  0)
    
    def p(w: List[Wire]) = w.map(_.toString).fold("")((next, acc) => acc + next)

  }
}
