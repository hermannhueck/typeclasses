package oofp.expr.fp._1

sealed trait Expr
final case class Lit(value: Double) extends Expr
final case class Plus(lhs: Expr, rhs: Expr) extends Expr

object Expr {

  def eval(e: Expr): Double = e match {
    case Lit(value) => value
    case Plus(lhs, rhs) => eval(lhs) + eval(rhs)
  }

  def show(e: Expr): String = e match {
    case Lit(value) => value.toString
    case Plus(lhs, rhs) => show(lhs) + " + " + show(rhs)
  }
}

object Main extends App {

  import Expr._

  val twoPlusThree: Expr = Plus(Lit(2), Lit(3))

  println(eval(twoPlusThree))
  println(show(twoPlusThree))
}