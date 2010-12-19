package expr

sealed abstract class Expr

case class Num(number: Double) extends Expr
case class Name(name: String) extends Expr
case class BinOp(op: Operator.Value, left: Expr, right: Expr) extends Expr

object Operator extends Enumeration {
  type Operator = Value
  val Mul = Value("*")
  val Add = Value("+")
  val Pow = Value("**")
}
