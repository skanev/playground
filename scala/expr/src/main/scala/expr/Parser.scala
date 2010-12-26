package expr

import scala.util.parsing.combinator._
import expr.BinOp._

object Parser {
  def parse(input: String): Expr = {
    val parser = new Parser()
    val result = parser.parseAll(parser.expr, input)

    result.getOrElse { throw new BadInputException(input) }
  }
}

class Parser extends JavaTokenParsers {
  def expr = addition
  def addition = binaryOp(BinOp.+, subtraction)
  def subtraction = binaryOp(BinOp.-, multiplication)
  def multiplication = binaryOp(*, division)
  def division = binaryOp(/, exp)
  def exp = binaryOp(^, factor)
  def factor: Parser[Expr] = "(" ~> expr <~ ")" | number | call | name;
  def call: Parser[Expr] = ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case name ~ "(" ~ args ~ ")" => Call(name, args: _*)
  }
  def number: Parser[Num] = floatingPointNumber ^^ (x => Num(x.toDouble))
  def name: Parser[Name] = ident ^^ Name

  private def binaryOp(operator: BinOp.Operator, operand: Parser[Expr]): Parser[Expr] = {
    val symbol = operator.toString;
    operand ~ symbol ~ operand ^^ { case left ~ symbol ~ right => BinOp(operator, left, right) } | operand
  }
}
