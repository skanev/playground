import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class UniformElementSpec extends Spec with ShouldMatchers {
  describe("UniformElement") {
    it("represents a block of text, filled with the same character") {
      new UniformElement('o', 3, 2).toString should equal ("ooo\nooo")
    }

    it("has the width it has been given") {
      new UniformElement('o', 3, 2).width should equal (3)
    }

    it("has the height it has been given") {
      new UniformElement('o', 3, 2).height should equal (2)
    }
  }
}
