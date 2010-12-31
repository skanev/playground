package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import BinOp.{Operator => O}

class LambdaSpec extends Spec with ShouldMatchers {
  val env = Env.empty
  val add = Lambda(List("X", "Y"), Name("X") + Name("Y"))

  it("can be evaluated with given parameters") {
    expect(3) { add.eval(env, List(1, 2)) }
  }

  it("can be printed as a string") {
    expect("lambda(X, Y) { X + Y }") { add.toString }
  }

  it("raises an error when invoked with the wrong number of arguments") {
    intercept[ExprException] { add.eval(env, List(1, 2, 3)) }
  }

  it("cannot be constructed with free variables") {
    intercept[ExprException] { Lambda(List("X"), Name("X") + Name("Y")) }
    intercept[ExprException] { Lambda(List("X"), Call("foo", List(Name("X"), Name("Y")))) }
  }
}
