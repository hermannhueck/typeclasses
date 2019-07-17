package oofp.expr.fp._2

sealed trait Expr
final case class Lit(value: Double) extends Expr
final case class Plus(e1: Expr, e2: Expr) extends Expr

object Expr {

  def fold[A](e: Expr)(lit: Double => A, plus: (Expr, Expr) => A): A = e match {
    case Lit(a) => lit(a)
    case Plus(lhs, rhs) => plus(lhs, rhs)
  }

  def eval(e: Expr): Double = fold(e)(identity, eval(_) + eval(_))

  def show(e: Expr): String = fold(e)(_.toString, show(_) + " + " + show(_))
}

object Main extends App {

  import Expr._

  val twoPlusThree: Expr = Plus(Lit(2), Lit(3))

  println(eval(twoPlusThree))
  println(show(twoPlusThree))
}