package layout

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import Element.elem

class CompositionSpec extends Spec with ShouldMatchers {
  it(".above should put one element above the other") {
    (elem("12") above elem("34")).toString should equal ("12\n34")
  }

  it(".beside should put two elements next to each other") {
    (elem(Array("1", "2")) beside elem(Array("3", "4"))).toString should equal ("13\n24")
  }
}
