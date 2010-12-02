package arithmetic

abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object Expr {
  def simplify: (Expr => Expr) = {
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e, Number(0)) => e
    case BinOp("*", e, Number(1)) => e
    case BinOp("*", e, Number(0)) => Number(0)
    case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
    case UnOp(o, e) => UnOp(o, simplify(e))
    case BinOp(o, l, r) => BinOp(o, simplify(l), simplify(r))
    case e => e
  }
}
