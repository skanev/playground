package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class ExpressionSpec extends Spec with ShouldMatchers {
  it("allows creating a binary operation with +") {
    expect(BinOp(Operator.+, Num(1), Num(2))) {
      Num(1) + Num(2)
    }
  }

  it("allows creating a binary operation with *") {
    expect(BinOp(Operator.*, Num(1), Num(2))) {
      Num(1) * Num(2)
    }
  }

  it("allows creating a BinOp with ^") {
    expect(BinOp(Operator.^, Num(1), Num(2))) {
      Num(1) ^ Num(2)
    }
  }

  it("allows creating a BinOp with -") {
    expect(BinOp(Operator.-, Num(1), Num(2))) {
      Num(1) - Num(2)
    }
  }
}
