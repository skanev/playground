package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class PrintingExpressionsSpec extends Spec with ShouldMatchers {
  it("prints 1 + 2") {
    BinOp("+", Num(1), Num(2)).toString should equal("1 + 2")
  }

  it("prints x ** 2 + 2 * x + 1") {
    val xx = BinOp("**", Name("x"), Num(2))
    val x2 = BinOp("*", Num(2), Name("x"))

    BinOp("+", xx, BinOp("+", x2, Num(1))).toString should equal("x ** 2 + 2 * x + 1")
  }

  it("prints (x + 2) * 3") {
    val expr = BinOp("*", BinOp("+", Name("x"), Num(2)), Num(3))
    expr.toString should equal ("(x + 2) * 3")
  }
}
