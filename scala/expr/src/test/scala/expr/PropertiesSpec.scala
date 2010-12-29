package expr

import org.scalatest.Spec
import org.scalatest.prop.Checkers
import org.scalatest.matchers.ShouldMatchers

import org.scalacheck._
import org.scalacheck.Prop._

import Printer.asString
import Expr.parse

class PropertiesSpec extends Spec with Checkers {
  object ExprProperties extends Properties("expr") {
    def sameResult(left: Double, right: Double): Boolean = (left == right) || (left.isNaN && right.isNaN)

    property("parse . toString is idemptotent (to some extent)") = forAll(ExprGen.expr) { expr =>
      expr == parse(asString(expr))
    }

    property("parse . toString evaluates to the same value") = forAll(ExprGen.expr) { expr =>
      sameResult(expr.eval(ExprGen.env), parse(asString(expr)).eval(ExprGen.env))
    }

    property("concurrent == normal evaluation") = forAll(ExprGen.expr) { expr =>
      sameResult(expr.eval(ExprGen.env), expr.ceval(ExprGen.env))
    }
  }

  it("satisfies its properties") { check(ExprProperties) }
}
