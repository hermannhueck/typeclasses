package oofp.expr.oo._2

trait ExprDsl {
  type Expr
  def lit(x: Double): Expr
  def plus(lhs: Expr, rhs: Expr): Expr
}

trait Eval extends ExprDsl {
  override type Expr = Double
  override def lit(x: Double): Expr = x
  override def plus(lhs: Expr, rhs: Expr): Expr = lhs + rhs
}

trait Show extends ExprDsl {
  override type Expr = String
  override def lit(x: Double): Expr = x.toString
  override def plus(lhs: Expr, rhs: Expr): Expr = lhs + " + " + rhs
}

trait Program extends ExprDsl {
  val twoPlusThree: Expr = plus(lit(2), lit(3))
}

object Main extends App with Eval {

  new Program with Eval {
    println(twoPlusThree)
  }

  new Program with Show {
    println(twoPlusThree)
  }
}
