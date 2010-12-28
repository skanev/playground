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

  it("parses 'X = 1 + 2' as an Assign(X, 1 + 2)") {
    expect(Assign("X", Num(1) + Num(2))) { Command.parse("X = 1 + 2") }
  }

  it("parses 'add = lambda(X, Y) { X + Y }' add Define(add, Lambda(..))") {
    val lambda = Lambda(List("X", "Y"), Name("X") + Name("Y"))
    expect(Define("add", lambda)) { Command.parse("add = lambda(X, Y) { X + Y }") }
  }

  it("throws an exception when command cannot be parsed") {
    intercept[BadInputException] { Command.parse("+++") }
  }
}
