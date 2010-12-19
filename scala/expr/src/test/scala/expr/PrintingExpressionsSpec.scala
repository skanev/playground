package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Operator._

class PrintingExpressionsSpec extends Spec with ShouldMatchers {
  def expectAsStringOf(expr: Expr)(expectation: => String) = {
    expect(ExprPrinter.asString(expr))(expectation)
  }

  it("prints 1 + 2") {
    val expr = BinOp(Add, Num(1), Num(2))

    expectAsStringOf(expr) { "1 + 2" }
  }

  it("prints x ** 2 + 2 * x + 1") {
    val expr = BinOp(Add, BinOp(Pow, Name("x"), Num(2)),
                          BinOp(Add, BinOp(Mul, Num(2), Name("x")),
                                     Num(1)))

    expectAsStringOf(expr) { "x ** 2 + 2 * x + 1" }
  }

  it("prints (x + 2) * 3") {
    val expr = BinOp(Mul, BinOp(Add, Name("x"), Num(2)), Num(3))

    expectAsStringOf(expr) { "(x + 2) * 3" }
  }
}
