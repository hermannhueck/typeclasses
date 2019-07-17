package oofp.expr.fp._3

sealed trait Expr
final case class Lit(value: Double) extends Expr
final case class Plus(e1: Expr, e2: Expr) extends Expr

object Expr {

  def fold[A](lit: Double => A, plus: (Expr, Expr) => A): Expr => A = {
    case Lit(a) => lit(a)
    case Plus(lhs, rhs) => plus(lhs, rhs)
  }

  val eval: Expr => Double = fold[Double](identity, eval(_) + eval(_))

  val show: Expr => String = fold[String](_.toString, show(_) + " + " + show(_))
}

object Main extends App {

  import Expr._

  val twoPlusThree: Expr = Plus(Lit(2), Lit(3))

  println(eval(twoPlusThree))
  println(show(twoPlusThree))
}