package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class EnvSpec extends Spec with ShouldMatchers {
  it("can be queried for values by name") {
    val env = new Env().withVariable("X", 1.0)
    expect(Env.Number(1.0)) { env("X") }
  }

  it("can be queried for variables") {
    val env = new Env().withVariable("X", 1.0)
    expect(1.0) { env.variable("X") }
  }

  it("can be extended with multiple variables") {
    val env = new Env().withVariables(Array("X" -> 1.0, "Y" -> 2.0))
    expect(1.0) { env.variable("X") }
    expect(2.0) { env.variable("Y") }
  }

  it("raises an error when queried for an unexisting name") {
    intercept[NoSuchElementException] { new Env()("X") }
  }
}
