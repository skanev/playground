package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class EnvSpec extends Spec with ShouldMatchers {
  it("can be constructed as an empty environment") {
    new Env()
  }

  it("can be constructed with a name-to-number mapping") {
    new Env("X" -> 1.0)
  }

  it("can be queried for values by name") {
    val env = new Env("X" -> 1.0)
    expect(Env.Number(1.0)) { env("X") }
  }

  it("can be queried for variables") {
    val env = new Env("X" -> 1.0)
    expect(1.0) { env.variable("X") }
  }

  it("raises an error when queried for an unexisting name") {
    intercept[NoSuchElementException] { new Env()("X") }
  }
}
