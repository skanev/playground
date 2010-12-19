package expr

sealed abstract class Expr {
  def +(other: Expr) = BinOp(Operator.+, this, other)
  def *(other: Expr) = BinOp(Operator.*, this, other)
  def ^(other: Expr) = BinOp(Operator.^, this, other)
}

case class Num(number: Double) extends Expr
case class Name(name: String) extends Expr
case class BinOp(op: Operator.Value, left: Expr, right: Expr) extends Expr

object Operator {
  sealed abstract class Value(symbol: String, val precendance: Int) {
    val operator = this

    def unapply(expr: Expr): Option[(Expr, Expr)] = {
      expr match {
        case BinOp(`operator`, left, right) => Some((left, right))
        case _ => None
      }
    }

    override def toString = symbol
  }

  object ^ extends Value("^", 1)
  object * extends Value("*", 2)
  object + extends Value("+", 3)
}
