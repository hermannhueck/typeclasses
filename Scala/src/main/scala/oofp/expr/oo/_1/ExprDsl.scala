package oofp.expr.oo._1

trait ExprDsl[Expr] {
  def lit(x: Double): Expr
  def plus(lhs: Expr, rhs: Expr): Expr
}

object ExprDsl {

  implicit object Eval extends ExprDsl[Double] {
    override def lit(x: Double): Double = x
    override def plus(lhs: Double, rhs: Double): Double = lhs + rhs
  }

  implicit object Show extends ExprDsl[String] {
    override def lit(x: Double): String = x.toString
    override def plus(lhs: String, rhs: String): String = lhs + " + " + rhs
  }
}

object Main extends App {

  def program[A](implicit e: ExprDsl[A]): A = {
    import e._
    plus(lit(20), lit(22))
  }

  println(program[Double])
  println(program[String])
}
