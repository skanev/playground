package expr

import org.scalatest.Spec
import org.scalatest.prop.Checkers
import org.scalatest.matchers.ShouldMatchers

import org.scalacheck.Prop._

import Expr.parse

class PropertiesSpec extends Spec with Checkers {
  def sameResult(left: Double, right: Double): Boolean = (left == right) || (left.isNaN && right.isNaN)

  describe("An expression") {
    it("is equal to its parsed toString") {
      check(forAll(ExprGen.expr) { expr => expr == parse(expr.toString) })
    }

    it("is evaluated to the same value as its parse . toString") {
      check(forAll(ExprGen.expr) { expr =>
        sameResult(expr.eval(ExprGen.env), parse(expr.toString).eval(ExprGen.env))
      })
    }

    it("evaluates to the same value with eval() and aeval()") {
      check(forAll(ExprGen.expr) { expr => sameResult(expr.eval(ExprGen.env), expr.aeval(ExprGen.env)) })
    }
  }
}
