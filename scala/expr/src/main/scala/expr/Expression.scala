package expr

sealed abstract class Expr {
  override def toString = ExprPrinter.makeString(this)
}
case class Num(number: Double) extends Expr
case class Name(name: String) extends Expr
case class BinOp(symbol: String, left: Expr, right: Expr) extends Expr

object ExprPrinter {
  private def precendance(operator: String): Int = {
    Map("**" -> 1, "*" -> 2, "+" -> 3)(operator)
  }

  private def parenthesize(expr: Expr, enclosingPrecendence: Int): String = {
    val str = makeString(expr)
    expr match {
      case binOp @ BinOp(symbol, _, _) =>
        if (precendance(symbol) > enclosingPrecendence) "(" + str + ")"
        else str
      case _ => str
    }
  }

  def makeString(expr: Expr): String = {
    expr match {
      case Num(number) => number.toString.replaceAll(".0$", "")
      case BinOp(symbol, left, right) =>
        val prec = precendance(symbol)
        parenthesize(left, prec) + " " + symbol + " " + parenthesize(right, prec)
      case Name(name) => name
    }
  }
}
