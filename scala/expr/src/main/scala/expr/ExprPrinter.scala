package expr

object ExprPrinter {
  private def precendance(operator: String): Int = {
    Map("**" -> 1, "*" -> 2, "+" -> 3)(operator)
  }

  private def parenthesize(expr: Expr, enclosingPrecendence: Int): String = {
    val str = asString(expr)
    expr match {
      case binOp @ BinOp(symbol, _, _) =>
        if (precendance(symbol) > enclosingPrecendence) "(" + str + ")"
        else str
      case _ => str
    }
  }

  def asString(expr: Expr): String = {
    expr match {
      case Num(number) => number.toString.replaceAll(".0$", "")
      case BinOp(symbol, left, right) =>
        val prec = precendance(symbol)
        parenthesize(left, prec) + " " + symbol + " " + parenthesize(right, prec)
      case Name(name) => name
    }
  }
}
