package oofp.calc.oo

class Calculator {
  def literal(v: Double): Double = v
  def add(x: Double, y: Double): Double = x + y
  def subtract(x: Double, y: Double): Double = x - y
  def multiply(x: Double, y: Double): Double = x * y
  def divide(x: Double, y: Double): Double = x / y
}

// OO style lets you easily add new operations
// operations = what we want to do
class TriginometricCalculator extends Calculator {
  def sin(v: Double): Double = Math.sin(v)
  def cos(v: Double): Double = Math.cos(v)
}

object Main extends App {

  val c = new Calculator
  import c._

  val result = add(literal(1.0), subtract(literal(3.0), literal(2.0)))
  println(result)

  val tc = new TriginometricCalculator
  import tc._

  val result2 = sin(result)
  println(result2)
}
