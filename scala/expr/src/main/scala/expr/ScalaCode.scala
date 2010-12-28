package expr

object ScalaCode {
  def define0(doc: String)(code: => Double): ScalaCode = define(doc) { case Nil => code }
  def define1(doc: String)(code: (Double) => Double): ScalaCode = define(doc) { case List(a) => code(a) }
  def define2(doc: String)(code: (Double, Double) => Double): ScalaCode = define(doc) { case List(a, b) => code(a, b) }
  def define3(doc: String)(code: (Double, Double, Double) => Double): ScalaCode = define(doc) { case List(a, b, c) => code(a, b, c) }

  private def define(doc: String)(code: PartialFunction[Seq[Double], Double]): ScalaCode = {
    def codeWithVerification(params: Seq[Double]): Double = {
      if (!code.isDefinedAt(params))
        throw new ExprException("Function called with an unexpected number of arguments")
      code(params)
    }
    new ScalaCode(doc, codeWithVerification)
  }
}

class ScalaCode(doc: String, code: Seq[Double] => Double) extends Callable {
  override def eval(env: Env, params: Seq[Double]): Double = code(params)
  override def toString = "<ScalaCode:%s>".format(doc)
}
