package arithmetic

import layout.Element
import layout.Element.elem

sealed abstract class Expr
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

class ExprFormatter {
  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )

  private val precendence = {
    val assocs =
      for {
        i <- 0 until opGroups.length
        op <- opGroups(i)
      } yield op -> i
    Map() ++ assocs
  }

  private val unaryPrecendence = opGroups.length
  private val fractionPrecendence = -1

  private def format(e: Expr, enclPrec: Int): Element =
    e match {
      case Var(name) => elem(name)
      case Number(num) => elem(num.toString.replaceAll(".0$", ""))
      case UnOp(op, arg) => elem(op) beside format(arg, unaryPrecendence)
      case BinOp("/", left, right) => 
        val top = format(left, fractionPrecendence)
        val bottom = format(right, fractionPrecendence)
        val line = elem('-', top.width max bottom.width, 1)
        val frac = top above line above bottom
        if (enclPrec != fractionPrecendence) frac
        else elem(" ") beside frac beside elem(" ")
      case BinOp(op, left, right) => 
        val opPrec = precendence(op)
        val l = format(left, opPrec)
        val r = format(right, opPrec)
        val oper = l beside elem(" " + op + " ") beside r
        if (enclPrec <= opPrec) oper
        else elem("(") beside oper beside elem(")")
    }

  def format(e: Expr): Element = format(e, 0)
}
