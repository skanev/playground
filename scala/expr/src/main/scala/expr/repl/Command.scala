package expr.repl

abstract sealed class Command

object Command {
  case class Exit() extends Command
  case class Eval(expr: Expr) extends Command

  def parse(input: String): Command = {
    val parser = new CommandParser
    val result = parser.parseAll(parser.command, input)

    result.getOrElse { throw new BadInputException("Cannot be parsed") }
  }

  class CommandParser extends expr.Parser {
    def command = exit | eval
    def exit: Parser[Command] = "exit" ^^ { case _ => Exit() }
    def eval = expr ^^ { case expr => Eval(expr) }
  }
}

