package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }

  override def toString = s"${if (getSignal) '1' else '0'}"
}

object TrueWire extends Wire {
  override def getSignal = true
}

abstract class CircuitSimulator extends Simulator {
  def b(b: Boolean) = if (b) '1' else '0'

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
        }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def andGate(in: List[Wire], out: Wire) {
    in match {
      case Nil =>
      case hd :: Nil => andGate(hd, TrueWire, out)
      case h1 :: h2 :: tl => {
        val o = new Wire
        andGate(h1, h2, o)

        andGate(o :: tl, out)
      }
    }

  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }

    a1 addAction orAction
    a2 addAction orAction
  }

  def nandGate(a1_p: Wire, a2_p: Wire, output_p: Wire) {
    val o = new Wire
    andGate(a1_p, a2_p, o)
    inverter(o, output_p)
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val o1, o2 = new Wire;

    nandGate(a1, a1, o1)
    nandGate(a2, a2, o2)
    nandGate(o1, o2, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => for (o <- out) {
        orGate(in, new Wire, o)
      }
      case hd :: tl => {
        val hdInv, oHigh, oLow = new Wire
        val (upper, lower) = out.splitAt(out.length / 2)

        inverter(hd, hdInv)
        andGate(in, hdInv, oLow)
        andGate(in, hd, oHigh)

        demux(oHigh, tl, lower)
        demux(oLow, tl, lower)
      }
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
