package expr.repl

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class REPLSpec extends Spec with ShouldMatchers {
  def shellWithInput(inputs: String*): RecordedShell = {
    val shell = new RecordedShell(inputs: _*)
    new REPL(shell).start()
    shell
  }

  def outputFor(input: String): String = {
    shellWithInput(input, "exit").messageBeforeExit
  }

  it("replies with 'Bye!' when you exit it") {
    expect("Bye!") { shellWithInput("exit").lastMessage }
  }

  it("evaluates an expression if given") {
    expect("= 3.0") { outputFor("1 + 2") }
  }
}
