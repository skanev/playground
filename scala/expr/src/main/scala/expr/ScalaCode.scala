package expr

object ScalaCode {
  def define0(code: => Double): ScalaCode = define { case Nil => code }
  def define1(code: (Double) => Double): ScalaCode = define { case List(a) => code(a) }
  def define2(code: (Double, Double) => Double): ScalaCode = define { case List(a, b) => code(a, b) }
  def define3(code: (Double, Double, Double) => Double): ScalaCode = define { case List(a, b, c) => code(a, b, c) }

  private def define(code: PartialFunction[Seq[Double], Double]): ScalaCode = {
    def codeWithVerification(params: Seq[Double]): Double = {
      if (!code.isDefinedAt(params))
        throw new IllegalArgumentException("Function called with an unexpected number of arguments")
      code(params)
    }
    new ScalaCode(codeWithVerification)
  }
}

class ScalaCode(val code: Seq[Double] => Double) extends Callable {
  override def eval(env: Env, params: Seq[Double]): Double = code(params)
}
