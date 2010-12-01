import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class LineElementSpec extends Spec with ShouldMatchers {
  describe("LineElement") {
    it("represents a single line of text") {
      new LineElement("12345").toString should equal ("12345")
    }

    it("is as wide as the string in it is long (obviously)") {
      new LineElement("12345").width should equal (5)
    }

    it("has height equal to 1") {
      new LineElement("something").height should equal (1)
    }
  }
}
