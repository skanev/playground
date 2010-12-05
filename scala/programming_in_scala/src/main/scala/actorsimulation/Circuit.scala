package actorsimulation

import scala.actors.Actor

class Circuit {
  val clock = new Clock

  val WireDelay = 1
  val InvertedDelay = 2
  val OrGateDelay = 3
  val AndGateDelay = 3

  case class SetSignal(sig: Boolean)
  case class SignalChanged(wire: Wire, sig: Boolean)

  class Wire(name: String, init: Boolean) extends Simulant {
    def this(name: String) { this(name, false) }
    def this() { this("unnamed") }

    val clock = Circuit.this.clock
    clock.add(this)

    private var sigVal = init
    private var observers: List[Actor] = List()

    def handleSimMessage(msg: Any) {
      msg match {
        case SetSignal(s) =>
          if (s != sigVal) {
            sigVal = s
            signalObservers()
          }
      }
    }

    def signalObservers() {
      for (observer <- observers)
        clock ! AfterDelay(WireDelay, SignalChanged(this, sigVal), observer)
    }

    override def simStarting() { signalObservers() }

    def addObserver(observer: Actor) {
      observers = observer :: observers
    }

    override def toString = "Wire(" + name + ")"
  }

  private object DummyWire extends Wire("dummy")

  abstract class Gate(in1: Wire, in2: Wire, out: Wire) extends Simulant {
    def computeOutput(s1: Boolean, s2: Boolean): Boolean
    val delay: Int

    val clock = Circuit.this.clock
    clock.add(this)

    in1.addObserver(this)
    in2.addObserver(this)

    var s1, s2 = false

    def handleSimMessage(msg: Any) {
      msg match {
        case SignalChanged(w, sig) => 
          if (w == in1) s1 = sig
          if (w == in2) s2 = sig
          clock ! AfterDelay(delay, SetSignal(computeOutput(s1, s2)), out)
      }
    }
  }

  def orGate(in1: Wire, in2: Wire, output: Wire) =
    new Gate(in1, in2, output) {
      val delay = OrGateDelay
      def computeOutput(s1: Boolean, s2: Boolean) = s1 || s2
    }

  def andGate(in1: Wire, in2: Wire, output: Wire) =
    new Gate(in1, in2, output) {
      val delay = AndGateDelay
      def computeOutput(s1: Boolean, s2: Boolean) = s1 && s2
    }

  def inverter(input: Wire, output: Wire) =
    new Gate(input, DummyWire, output) {
      val delay = InvertedDelay
      def computeOutput(s1: Boolean, s2: Boolean) = !s1
    }

  def probe(wire: Wire) = new Simulant {
    val clock = Circuit.this.clock
    clock.add(this)
    wire.addObserver(this)
    def handleSimMessage(msg: Any) {
      msg match {
        case SignalChanged(wire, signalValue) =>
          println("singal " + wire + " changed to " + signalValue)
      }
    }
  }

  def start() { clock ! Start }
}
