package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class ScalaCodeSpec extends Spec with ShouldMatchers {
  it("allows easy construction of functions of no arguments") {
    val code = ScalaCode.define0 { 42 }
    expect(42) { code.eval(new Env(), List()) }
  }

  it("allows easy construction of functions of one argument") {
    val code = ScalaCode.define1 { a => a * 2 }
    expect(4) { code.eval(new Env(), List(2)) }
  }

  it("allows easy construction of functions of two arguments") {
    val code = ScalaCode.define2 { (a, b) => a + b }
    expect(3) { code.eval(new Env(), List(1, 2)) }
  }

  it("allows easy construction of functions of three arguments") {
    val code = ScalaCode.define3 { (a, b, c) => a + b * c }
    expect(7) { code.eval(new Env(), List(1, 2, 3)) }
  }

  it("raises an error when not invoked with the right number of arguments") {
    val code = ScalaCode.define2 { (a, b) => a + b }
    intercept[IllegalArgumentException] { code.eval(new Env(), List(1, 2, 3)) }
  }
}
