package expr.repl

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class REPLSpec extends Spec with ShouldMatchers {
  def lastMessageInSession(inputs: String*): String = {
    val shell = new RecordedShell(inputs ++ List("exit"): _*)
    new REPL(shell).start()
    shell.lastMessage
  }

  it("evaluates an expression if given") {
    expect("= 3.0") { lastMessageInSession("1 + 2") }
  }

  it("evaluates expressions in the defined context") {
    expect("= 3.0") { lastMessageInSession("X = 1", "Y = 2", "X + Y") }
  }

  it("displays an error when given an unparsable expression") {
    expect("ERROR: Unparsable input") { lastMessageInSession("+++") }
  }

  it("displays an error when refering to an unknown variable") {
    expect("ERROR: Undefined variable: X") { lastMessageInSession("X + 1") }
  }
}
