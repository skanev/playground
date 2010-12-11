package expr

sealed abstract class Expr

case class Num(number: Double) extends Expr
case class Name(name: String) extends Expr
case class BinOp(symbol: String, left: Expr, right: Expr) extends Expr
