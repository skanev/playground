package arithmetic

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Expr.simplify

class SimplificationSpec extends Spec with ShouldMatchers {
  describe("Expr.simplify(expr)") {
    it("simplifies -(-1)) to 1") {
      simplify(UnOp("-", UnOp("-", Number(1)))) should equal (Number(1))
    }

    it("simplifies e+0 to e") {
      simplify(BinOp("+", Var("e"), Number(0))) should equal (Var("e"))
    }

    it("simplifies e*1 to e") {
      simplify(BinOp("*", Var("e"), Number(1))) should equal (Var("e"))
    }

    it("simplifies e*0 to 0") {
      simplify(BinOp("*", Var("e"), Number(0))) should equal (Number(0))
    }

    it("'simplifies' (a+b)+(a+b) to (a+b)*2") {
      simplify(BinOp("+",
        BinOp("+", Var("a"), Var("b")),
        BinOp("+", Var("a"), Var("b"))
      )) should equal (BinOp("*",
        BinOp("+", Var("a"), Var("b")),
        Number(2)))
    }

    it("simplifies log((-(-1)) + (x+0)) to log(1)") {
      simplify(UnOp("log",
        BinOp("+",
          UnOp("-", UnOp("-", Number(1))),
          BinOp("+", Var("x"), Number(0))))
      ) should equal (UnOp("log", BinOp("+", Number(1), Var("x"))))
    }
  }
}
