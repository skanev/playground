package expr

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary

import expr.BinOp._

object ExprGen {
  def eval(e: Expr) = Evaluator.eval(e, env)

  val env = Env.empty
    .extend("x", 1.44)
    .extend("e", 2.71)
    .extend("add", Lambda(List("x", "y"), Name("x") + Name("y")))
    .extend("twice", Lambda(List("x"), Name("x") + Name("x")))
    .extend("pi", ScalaCode.define0("pi()") { Math.Pi })

  val number = arbitrary[Double].map(Num(_))
  val name = oneOf("x", "e").map(Name(_))

  def binOp: Gen[Expr] = (for {
    op <- oneOf(BinOp.+, BinOp.-, BinOp.*, BinOp./, BinOp.^)
    left <- oneOf(binOp, call, number, name)
    right <- oneOf(binOp, call, number, name)
  } yield BinOp(op, left, right)) suchThat {
    case BinOp(op1, _, BinOp(op2, _, _)) if op1 == op2 => false
    case _ => true
  }

  def call: Gen[Expr] = for {
    a <- oneOf(binOp, number, name)
    b <- oneOf(binOp, number, name)
    fun <- oneOf("pi", "twice", "add")
  } yield (fun match {
    case "pi" => Call("pi", List())
    case "twice" => Call("twice", List(a))
    case "add" => Call("add", List(a, b))
  })

  def expr: Gen[Expr] = binOp
}
