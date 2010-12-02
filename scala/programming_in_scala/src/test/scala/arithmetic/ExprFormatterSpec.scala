package arithmetic

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class ExprFormatterSpec extends Spec with ShouldMatchers {
  describe("ExprFormatter.format") {
    def draw(e: Expr): String = new ExprFormatter().format(e).toString

    it("can draw a + (b + c)") {
      draw(BinOp("+", Var("a"), BinOp("+", Var("b"), Var("c")))) should equal ("a + b + c")
    }

    it("can draw (a / b) / c") {
      draw(BinOp("/", BinOp("/", Var("a"), Var("b")), Var("c"))) should equal (
        """| a 
           | - 
           | b 
           |---
           | c """.stripMargin
      )
    }

    it("can draw x / x + 1") {
      draw(BinOp("/", Var("x"), BinOp("+", Var("x"), Number(1)))) should equal (
        """|  x  
           |-----
           |x + 1""".stripMargin
      )
    }

    it("can draw ((a / (b * c) + 1 / n) / 3)") {
      draw(BinOp("/", BinOp("+", BinOp("/", Var("a"), BinOp("*", Var("b"), Var("c"))),
                                 BinOp("/", Number(1), Var("n"))),
                      Number(3))
      ) should equal (
        """|  a     1
           |----- + -
           |b * c   n
           |---------
           |    3    """.stripMargin
      )
    }

    it("can draw ((1 / 2) * (x + 1)) / ((x / 2) + (1.5 / x))") {
      draw(BinOp("/", BinOp("*", BinOp("/", Number(1), Number(2)),
                                 BinOp("+", Var("x"), Number(1))),
                      BinOp("+", BinOp("/", Var("x"), Number(2)),
                                 BinOp("/", Number(1.5), Var("x"))))
      ) should equal (
        """|1          
           |- * (x + 1)
           |2          
           |-----------
           |  x   1.5  
           |  - + ---  
           |  2    x   """.stripMargin
      )
    }
  }
}
