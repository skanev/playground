package expr

class Function(val name: String, val args: Array[String], val expr: Expr) {
  verifyNoFreeVariables()

  def arity = args.length
  def eval(params: Double*): Double = {
    verifyArity(params.length)
    val env = Map() ++ args.zip(params.toArray)
    Evaluator.eval(expr, env)
  }

  private def freeVariables(expr: Expr): Seq[String] = {
    expr match {
      case BinOp(_, left, right) => freeVariables(left) ++ freeVariables(right)
      case Call(_, params @ _*) => params.flatMap(freeVariables)
      case Name(name) if !args.contains(name) => List(name)
      case _ => List()
    }
  }

  private def verifyArity(paramCount: Int) {
    if (paramCount != arity)
      throw new IllegalArgumentException("Function " + name + " expects " + arity + "arguments, " +
          "but was called with " + paramCount)
  }

  private def verifyNoFreeVariables() = {
    val vars = freeVariables(expr)
    if (vars.length != 0)
      throw new IllegalArgumentException("Function contains free variables: " + vars.mkString(","))
  }
}
