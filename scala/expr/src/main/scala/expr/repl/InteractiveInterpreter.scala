package expr.repl

import Expr.parse

object InteractiveInterpreter extends Application {
  val env = Env.empty
    .extend("PI", Math.Pi)
    .extend("E", Math.E)
    .extend("sin", ScalaCode.define1("sin(x) in radians") { x => Math.sin(x) })
    .extend("cos", ScalaCode.define1("cos(x) in radians") { x => Math.cos(x) })
    .extend("ln", ScalaCode.define1("ln(x) -- log with base e") { x => Math.log(x) })
    .extend("one", Lambda(List("x"), parse("sin(x) ^ 2 + cos(x) ^ 2")))
    .extend("log", Lambda(List("base", "number"), parse("ln(number) / ln(base)")))

  println("Type 'help' for help.")
  new REPL(ConsoleShell, env).start()
}
