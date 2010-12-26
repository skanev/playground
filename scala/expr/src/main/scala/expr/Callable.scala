package expr

trait Callable {
  def eval(env: Env, params: Seq[Double]): Double
}
