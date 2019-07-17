package oofp.calc.fp

sealed trait Calculation {
}

final case class Literal(v: Double) extends Calculation
final case class Add(a: Calculation, b: Calculation) extends Calculation
final case class Subtract(a: Calculation, b: Calculation) extends Calculation
final case class Multiply(a: Calculation, b: Calculation) extends Calculation
final case class Divide(a: Calculation, b: Calculation) extends Calculation

object Calculation {

  def eval(c: Calculation): Double = c match {
    case Literal(v: Double) => v
    case Add(a, b) => eval(a) + eval(b)
    case Subtract(a, b) => eval(a) - eval(b)
    case Multiply(a, b) => eval(a) * eval(b)
    case Divide(a, b) => eval(a) / eval(b)
  }

  // FP style lets you easily add new actions/interpreters
  // operations = how we want to do it
  def pretty(c: Calculation): String = c match {
    case Literal(v: Double) => v.toString
    case Add(a, b) => s"${pretty(a)} + ${pretty(b)}"
    case Subtract(a, b) => s"${pretty(a)} - ${pretty(b)}"
    case Multiply(a, b) => s"${pretty(a)} * ${pretty(b)}"
    case Divide(a, b) => s"${pretty(a)} / ${pretty(b)}"
  }
}
