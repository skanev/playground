package expr

class Lambda(val args: Array[String], val expr: Expr) extends Callable {
  verifyNoFreeVariables()

  def arity = args.length
  override def eval(env: Env, params: Seq[Double]): Double = {
    verifyArity(params.length)
    val extended = env.extend(args zip params.toArray)
    Evaluator.eval(expr, extended)
  }

  private def freeVariables(expr: Expr): Seq[String] = {
    expr match {
      case BinOp(_, left, right) => freeVariables(left) ++ freeVariables(right)
      case Call(_, params) => params.flatMap(freeVariables)
      case Name(name) if !args.contains(name) => List(name)
      case _ => List()
    }
  }

  override def toString = "lambda(%s) { %s }".format(args.mkString(", "), Printer.asString(expr))

  private def verifyArity(paramCount: Int) {
    if (paramCount != arity)
      throw new IllegalArgumentException("Lambda expects " + arity + "arguments, " +
          "but was called with " + paramCount)
  }

  private def verifyNoFreeVariables() = {
    val vars = freeVariables(expr)
    if (vars.length != 0)
      throw new IllegalArgumentException("Lambda contains free variables: " + vars.mkString(","))
  }
}
