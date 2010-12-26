package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import BinOp.{Operator => O}

class LambdaSpec extends Spec with ShouldMatchers {
  val env = new Env()
  val add = new Lambda(Array("X", "Y"), Name("X") + Name("Y"))

  it("knows its arity") {
    expect(2) { add.arity }
  }

  it("can be evaluated with given parameters") {
    expect(3) { add.eval(env, List(1, 2)) }
  }

  it("can be printed as a string") {
    expect("lambda(X, Y) { X + Y }") { add.toString }
  }

  it("raises an error when invoked with the wrong number of arguments") {
    intercept[IllegalArgumentException] { add.eval(env, List(1, 2, 3)) }
  }

  it("cannot be constructed with free variables") {
    intercept[IllegalArgumentException] { new Lambda(Array("X"), Name("X") + Name("Y")) }
    intercept[IllegalArgumentException] { new Lambda(Array("X"), Call("foo", List(Name("X"), Name("Y")))) }
  }
}
