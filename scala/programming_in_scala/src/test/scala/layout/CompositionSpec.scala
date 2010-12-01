package layout

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import Element.elem

class CompositionSpec extends Spec with ShouldMatchers {
  it(".widen(w: Int) pads the element with spaces until it reaches width w") {
    elem("x").widen(3).toString should equal (" x ")
    elem("x").widen(4).toString should equal (" x  ")
    elem(Array("1", "2")).widen(3).toString should equal(" 1 \n 2 ")
  }

  it(".heighten(h: Int) pads the element with spaces until it reaches height h") {
    elem("x").heighten(3).toString should equal (" \nx\n ")
    elem("x").heighten(2).toString should equal ("x\n ")
    elem("12345").heighten(2).toString should equal ("""|12345
                                                        |     """.stripMargin)
  }

  describe(".above") {
    it("put one element above the other") {
      (elem("12") above elem("34")).toString should equal ("12\n34")
    }

    it("allows for elements of different size") {
      val composition = elem("1234").above(elem("12")).toString
      composition should equal ("""|1234
                                   | 12 """.stripMargin)
    }
  }

  describe(".beside") {
    it("puts two elements next to each other") {
      (elem(Array("1", "2")) beside elem(Array("3", "4"))).toString should equal ("13\n24")
    }

    it("allows for elements of different sizes") {
      val composition = elem("one ").beside(elem(Array("one", "two"))).toString
      composition should equal ("""|one one
                                   |    two""".stripMargin)
    }
  }
}
