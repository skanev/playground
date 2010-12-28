package expr.repl

import expr.BadInputException
import Command._
import Evaluator.eval

class REPL(val shell: Shell) {
  var env = Env.empty

  def start() = processNextLine()

  private def processNextLine() {
    try {
      parse(shell.read) match {
        case Eval(expr) => shell.writeln("= " + eval(expr, env))
        case Assign(name, expr) => env = env.extend(name, eval(expr, env))
        case Define(name, lambda) => env = env.extend(name, lambda)
        case ShowEnv() => listNames()
        case Exit() => return
      }
    } catch {
      case ex: BadInputException => shell.writeln("ERROR: Unparsable input")
      case ex: UndefinedNameException => shell.writeln("ERROR: " + ex.message)
    }
    processNextLine()
  }

  private def listNames() {
    for (name <- env.names) {
      val repr = env(name).repr
      shell.writeln("%s = %s".format(name, repr))
    }
  }
}
