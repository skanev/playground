package expr.repl

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class REPLSpec extends Spec with ShouldMatchers {
  def shellWithInput(inputs: String*): RecordedShell = {
    val shell = new RecordedShell(inputs: _*)
    new REPL(shell).start()
    shell
  }

  def lastMessageInSession(inputs: String*): String = {
    shellWithInput(inputs ++ List("exit"): _*).messageBeforeExit
  }

  it("replies with 'Bye!' when you exit it") {
    expect("Bye!") { shellWithInput("exit").lastMessage }
  }

  it("evaluates an expression if given") {
    expect("= 3.0") { lastMessageInSession("1 + 2") }
  }

  it("evaluates expressions in the defined context") {
    expect("= 3.0") { lastMessageInSession("X = 1", "Y = 2", "X + Y") }
  }
}
