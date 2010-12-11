package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class PrintingExpressionsSpec extends Spec with ShouldMatchers {
  def expectAsStringOf(expr: Expr)(expectation: => String) = {
    expect(ExprPrinter.asString(expr))(expectation)
  }

  it("prints 1 + 2") {
    val expr = BinOp("+", Num(1), Num(2))

    expectAsStringOf(expr) { "1 + 2" }
  }

  it("prints x ** 2 + 2 * x + 1") {
    val expr = BinOp("+", BinOp("**", Name("x"), Num(2)),
                          BinOp("+", BinOp("*", Num(2), Name("x")),
                                     Num(1)))

    expectAsStringOf(expr) { "x ** 2 + 2 * x + 1" }
  }

  it("prints (x + 2) * 3") {
    val expr = BinOp("*", BinOp("+", Name("x"), Num(2)), Num(3))

    expectAsStringOf(expr) { "(x + 2) * 3" }
  }
}
