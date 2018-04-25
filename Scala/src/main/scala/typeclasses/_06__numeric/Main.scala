package typeclasses._06__numeric

import typeclasses._06__numeric.domain.Amount

object Main extends App {

  println

  val amounts: List[Amount] = List(Amount(34.86), Amount(219.88), Amount(52.71), Amount(75.83), Amount(127.29))
  println(amounts)

  // A list of Amounts can be summed because amountNumeric is in implicit scope.
  // It is visible as it is located in the companion object of case class Amount.
  //
  val sum: Amount = amounts.sum
  println(sum)

  println
}
