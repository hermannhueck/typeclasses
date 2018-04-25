package typeclasses._06__numeric.domain

final case class Amount(value: Double)

object Amount {

  // Putting implicit amountNumeric into the companion object
  // makes it available/visible in implicit scope.
  //
  implicit val amountNumeric: Numeric[Amount] = new Numeric[Amount] {

    override def plus(x: Amount, y: Amount): Amount = Amount(x.value + y.value)

    override def minus(x: Amount, y: Amount): Amount = Amount(x.value + y.value)

    override def times(x: Amount, y: Amount): Amount = Amount(x.value * y.value)

    override def negate(x: Amount): Amount =  Amount(-x.value)

    override def fromInt(x: Int): Amount = Amount(x.toLong)

    override def toInt(x: Amount): Int = x.value.toInt

    override def toLong(x: Amount): Long = x.value.toLong

    override def toFloat(x: Amount): Float = x.value.toFloat

    override def toDouble(x: Amount): Double = x.value

    override def compare(x: Amount, y: Amount): Int = (x.value, y.value) match {
      case (xv, yv) => if (xv < yv) -1 else if (xv > yv) 1 else 0
    }
  }
}
