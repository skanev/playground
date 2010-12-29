package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class PrintingExpressionsSpec extends Spec with ShouldMatchers {
  val examples = Array(
    Num(1) + Num(2) -> "1 + 2"
    , (Name("x") ^ Num(2)) + Num(2) * Name("x") + Num(1) -> "x ^ 2 + 2 * x + 1"
    , (Name("x") + Num(2)) * Num(3) -> "(x + 2) * 3"
    , (Num(1) - Num(2)) + (Num(3) - Num(4)) -> "1 - 2 + 3 - 4"
    , (Call("add", Array(Num(1), Num(2)))) -> "add(1, 2)"
    , (Num(1) - (Num(2) + Num(3))) -> "1 - (2 + 3)"
  )

  for ((expr, string) <- examples) {
    it("prints " + string) {
      expect(string) { expr.toString }
    }
  }
}
