package expr.repl

object InteractiveInterpreter extends Application {
  new REPL(ConsoleShell).start()
}
