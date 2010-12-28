package expr.repl

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Command._

class CommandSpec extends Spec with ShouldMatchers {
  it("parses 'exit' as the Exit() command") {
    expect(Exit()) { Command.parse("exit") }
  }

  it("parses '1 + 2' as an Eval(1 + 2)") {
    expect(Eval(Num(1) + Num(2))) { Command.parse("1 + 2") }
  }

  it("throws an exception when command cannot be parsed") {
    intercept[BadInputException] { Command.parse("+++") }
  }
}
