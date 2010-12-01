package layout

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class ArrayElementSpec extends Spec with ShouldMatchers {
  describe("ArrayElement") {
    it("represents multiple lines of text with the same height") {
      new ArrayElement(Array("aaa", "bbb", "ccc")).toString should equal ("aaa\nbbb\nccc")
    }

    it("is as wide as the first string in it") {
      new ArrayElement(Array("12345", "54321")).width should equal (5)
    }

    it("has width 0 when empty") {
      new ArrayElement(Array()).width should equal (0)
    }

    it("has height equal to the number of strings in it") {
      new ArrayElement(Array("1", "2", "3")).height should equal (3)
    }
  }
}
