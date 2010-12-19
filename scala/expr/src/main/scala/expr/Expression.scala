package expr

sealed abstract class Expr

case class Num(number: Double) extends Expr
case class Name(name: String) extends Expr
case class BinOp(op: Operator.Value, left: Expr, right: Expr) extends Expr

object Operator {
  sealed abstract class Value(symbol: String) {
    override def toString = symbol
  }
  object * extends Value("*")
  object + extends Value("+")
  object ** extends Value("**")
}
