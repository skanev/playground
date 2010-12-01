package layout

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import Element.elem

class FactoryMethodsSpec extends Spec with ShouldMatchers {
  describe("Element.elem(lines: Array[String]") {
    it("constructs an element from multiple lines of text") {
      elem(Array("aaa", "bbb", "ccc")).toString should equal ("aaa\nbbb\nccc")
    }

    it("constructs an element as wide as the first string in lines") {
      elem(Array("12345", "54321")).width should equal (5)
    }

    it("constructs an element with width equal to 0 if lines is empty") {
      elem(Array[String]()).width should equal (0)
    }

    it("constructs an element with height equal to the length of lines") {
      elem(Array("1", "2", "3")).height should equal (3)
    }
  }

  describe("Element.elem(line: String") {
    it("constructs a one-row element from line") {
      elem("12345").toString should equal ("12345")
    }

    it("constructs an element as wide as line") {
      elem("12345").width should equal (5)
    }

    it("constructs an element with height 1") {
      elem("something").height should equal (1)
    }
  }

  describe("Element.elem(fill: Char, width: Int, height: Int)") {
    it("constructs a width x height block, filled uniformly with fill") {
      elem('o', 3, 2).toString should equal ("ooo\nooo")
    }

    it("constructs an element with the given width") {
      elem('o', 3, 2).width should equal (3)
    }

    it("constructs an element with the given height") {
      elem('o', 3, 2).height should equal (2)
    }
  }
}
