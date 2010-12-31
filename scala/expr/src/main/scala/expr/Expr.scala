package expr

object Expr {
  def parse(input: String) = Parser.parse(input)
}

sealed abstract class Expr {
  def +(other: Expr) = BinOp(BinOp.+, this, other)
  def -(other: Expr) = BinOp(BinOp.-, this, other)
  def *(other: Expr) = BinOp(BinOp.*, this, other)
  def ^(other: Expr) = BinOp(BinOp.^, this, other)

  def eval(env: Env) = Evaluation.eval(this, env)
  def aeval(env: Env) = ActorEvaluation.eval(this, env)

  override def toString = Printer.asString(this)
}

case class Num(number: Double) extends Expr
case class Name(name: String) extends Expr
case class BinOp(op: BinOp.Operator, left: Expr, right: Expr) extends Expr
case class Call(name: String, arguments: Seq[Expr]) extends Expr

object BinOp {
  sealed abstract class Operator(symbol: String, val precendance: Int) {
    val operator = this

    def unapply(expr: Expr): Option[(Expr, Expr)] = {
      expr match {
        case BinOp(`operator`, left, right) => Some((left, right))
        case _ => None
      }
    }

    override def toString = symbol
  }

  object ^ extends Operator("^", 1)
  object / extends Operator("/", 2)
  object * extends Operator("*", 3)
  object - extends Operator("-", 4)
  object + extends Operator("+", 5)
}
