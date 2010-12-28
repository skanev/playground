package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class EnvSpec extends Spec with ShouldMatchers {
  it("can be extended with a variable") {
    val env = Env.empty.extend("X", 1.0)
    expect(1.0) { env.variable("X") }
  }

  it("can be extended with multiple variables") {
    val env = Env.empty.extend(Array("X" -> 1.0, "Y" -> 2.0))
    expect(1.0) { env.variable("X") }
    expect(2.0) { env.variable("Y") }
  }

  it("overrides existing variables") {
    val env = Env.empty.extend("X", 1)
    expect(2) { env.extend("X", 2).variable("X") }
    expect(2) { env.extend(Array("X" -> 2)).variable("X") }
  }

  it("can be extended with a functions") {
    val add = Lambda(List("X", "Y"), Name("X") + Name("Y"))
    val env = Env.empty.extend("add", add)
    expect(add) { env.function("add") }
  }

  it("raises an error when queried for an unexisting name") {
    intercept[ExprException] { Env.empty.variable("X") }
    intercept[ExprException] { Env.empty.function("X") }
  }

  it("can tell its own bound names") {
    val env = Env.empty.extend("X", 1).extend("foo", Lambda(List(), Num(1)))
    expect(Set("X", "foo")) { env.names }
  }
}
