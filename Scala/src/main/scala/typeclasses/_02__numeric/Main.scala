package typeclasses._02__numeric

import typeclasses._02__numeric.domain.Amount

object Main extends App {

  implicit val stringNumeric: Numeric[String] = new Numeric[String] {

    // zero is needed for the concat of an empty list
    override def zero: String = ""
    override def plus(x: String, y: String): String = x + y

    // these methods are not used in our concat example for List[String]
    // hence not imnplemented. It's just for demo purposes.
    override def minus(x: String, y: String): String = ???
    override def times(x: String, y: String): String = ???
    override def negate(x: String): String = ???
    override def fromInt(x: Int): String = ???
    override def toInt(x: String): Int = ???
    override def toLong(x: String): Long = ???
    override def toFloat(x: String): Float = ???
    override def toDouble(x: String): Double = ???
    override def compare(x: String, y: String): Int = ???
  }

  println("-----")

  val strings = List("Type", "classes", "are", "a", "great", "design", "pattern.").flatMap(s => List(s, " "))
  println(strings)

  val sumStrings = strings.sum
  println(sumStrings.trim)

  println("-----")

  val amounts: List[Amount] = List(Amount(34.86), Amount(219.88), Amount(52.71), Amount(75.83), Amount(127.29))
  println(amounts)

  // A list of Amounts can be summed because amountNumeric is in implicit scope.
  // It is visible as it is located in the companion object of case class Amount.
  //
  val sumAmounts: Amount = amounts.sum
  println(sumAmounts)

  println("-----")
}
