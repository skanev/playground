package expr

case class Lambda(args: List[String], expr: Expr) extends Callable {
  verifyNoFreeVariables()

  override def toString = "lambda(%s) { %s }".format(args.mkString(", "), expr.toString)

  override def eval(env: Env, params: Seq[Double]): Double = {
    verifyArity(params.length)
    val extended = env.extend(args zip params.toList)
    expr.eval(extended)
  }

  private def freeVariables(expr: Expr): Seq[String] = {
    expr match {
      case BinOp(_, left, right) => freeVariables(left) ++ freeVariables(right)
      case Call(_, params) => params.flatMap(freeVariables)
      case Name(name) if !args.contains(name) => List(name)
      case _ => List()
    }
  }

  private def verifyArity(paramCount: Int) {
    if (paramCount != args.length)
      throw new ExprException("Lambda expects " + args.length + " argument(s), " +
          "but was called with " + paramCount)
  }

  private def verifyNoFreeVariables() = {
    val vars = freeVariables(expr)
    if (vars.length != 0)
      throw new ExprException("Lambda contains free variables: " + vars.mkString(","))
  }
}
