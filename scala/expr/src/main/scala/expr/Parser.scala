package expr

import scala.util.parsing.combinator._
import Operator._

object Parser {
  def parse(input: String): Expr = {
    val parser = new Parser()
    val result = parser.parseAll(parser.expr, input)

    result.getOrElse { throw new BadInputException(input) }
  }
}

class Parser extends JavaTokenParsers {
  def expr = binaryOp(Add, term)
  def term = binaryOp(Mul, exp)
  def exp = binaryOp(Pow, factor)
  def factor: Parser[Expr] = "(" ~> expr <~ ")" | number | name
  def number: Parser[Num] = floatingPointNumber ^^ (x => Num(x.toDouble))
  def name: Parser[Name] = ident ^^ Name

  private def binaryOp(operator: Operator.Value, operand: Parser[Expr]): Parser[Expr] = {
    val symbol = operator.toString;
    operand ~ symbol ~ operand ^^ { case left ~ symbol ~ right => BinOp(operator, left, right) } | operand
  }
}
