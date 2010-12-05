package actorsimulation

object Demo {
  def main(args: Array[String]) {
    val circuit = new Circuit with Adders
    import circuit._

    val ain = new Wire("ain", true)
    val bin = new Wire("bin", false)
    val cin = new Wire("cin", true)
    val sout = new Wire("sout")
    val cout = new Wire("cout")

    probe(ain)
    probe(bin)
    probe(cin)
    probe(sout)
    probe(cout)

    fullAdder(ain, bin, cin, sout, cout)

    circuit.start()
  }
}
