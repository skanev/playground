package expr

object Printer {
  private def parenthesize(expr: Expr, enclosingPrecendence: Int): String = {
    val str = asString(expr)
    expr match {
      case BinOp(operator, _, _) if operator.precendance > enclosingPrecendence =>
        "(" + str + ")"
      case _ => str
    }
  }

  def asString(expr: Expr): String = {
    expr match {
      case Num(number) => number.toString.replaceAll(".0$", "")
      case Name(name) => name
      case Call(name, args) => "%s(%s)".format(name, args.map(asString).mkString(", "))
      case BinOp(operator, left, right) =>
        val enclosing = operator.precendance
        parenthesize(left, enclosing) + " " + operator + " " + parenthesize(right, enclosing)
    }
  }
}
