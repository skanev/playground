package layout

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import Element.elem

class FactoryMethodsSpec extends Spec with ShouldMatchers {
  describe("Factory methods") {
    it(".elem(lines: Array[String] creates an ArrayElement") {
      elem(Array("x", "y")).toString should equal ("x\ny")
    }

    it(".elem(line: String) creates a LineElement") {
      elem("line").toString should equal ("line")
    }

    it(".elem(fill: Char, width: Int, height: Int) creates a UniformElement") {
      elem('o', 3, 2).toString should equal ("ooo\nooo")
    }
  }
}
