package expr.repl

import expr.BadInputException
import Command._

class REPL(shell: Shell, private var env: Env) {
  def this(shell: Shell) { this(shell, Env.empty) }

  def start() = processNextLine()

  private def processNextLine() {
    try {
      parse(shell.read) match {
        case Eval(expr) => shell.writeln("= " + expr.eval(env))
        case Assign(name, expr) => env = env.extend(name, expr.eval(env))
        case Define(name, lambda) => env = env.extend(name, lambda)
        case ShowEnv() => listNames()
        case Help() => showHelp()
        case Exit() => return
      }
    } catch {
      case ex: BadInputException => shell.writeln("ERROR: Unparsable input")
      case ex: ExprException => shell.writeln("ERROR: " + ex.message)
    }
    processNextLine()
  }

  private def listNames() {
    if (env.names.isEmpty) return

    val width = env.names.toList.map(_.length).sort(_ > _).first
    def pad(name: String) = name + " " * (width - name.length)

    for (name <- env.names) {
      shell.writeln("%s = %s".format(pad(name), env(name).repr))
    }
  }

  private def showHelp() {
    shell.writeln("""|Usage instructions:
                     |  * write any expression in order to evaluate it:
                     |
                     |     1 + 2 + 3 + 5 + 7 + 11 + 13
                     |     X + add(2, 4)
                     |
                     |  * assign variables or define functions with =
                     |
                     |     ANSWER = 42
                     |     add = lambda(X, Y) { X + Y }
                     |
                     |  * other available commands
                     |
                     |     names -- show all defined names
                     |     exit  -- quit the interpreter
                     |     help  -- you are looking at it""".stripMargin)
  }
}
