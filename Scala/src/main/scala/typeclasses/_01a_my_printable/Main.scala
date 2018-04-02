package typeclasses._01a_my_printable

import java.util.Date

import typeclasses._01a_my_printable.domain.Cat
import typeclasses._01a_my_printable.libPrintable.Printable
import Printable.instances._

object Main extends App {

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    // overrides PrintableInstances.intPrintable which is in scope but has lower priority
    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      override def stringify(value: Int): String = "How many cats? " + value.toString
    }

    // overrides PrintableInstances.datePrintable which is in scope but has lower priority
    implicit val datePrintable: Printable[Date] = new Printable[Date] {
      override def stringify(value: Date): String = "Date of meeting: " + value.toString
    }

    def myPrint[A](value: A)(implicit printable: Printable[A]): Unit =
      println(printable.stringify(value))

    println("\n-----")

    println("---> Using the local method 'myPrint' and local Printable instances to print ...")

    myPrint("Cats are meeting here!")
    myPrint(2)
    myPrint(false)
    myPrint(new Date)
    myPrint(mizzi)
    myPrint(garfield)
  }

  {
    println("---> Using the Printable companion object to stringify and print ...")

    println(Printable.stringify("Cats are meeting here!"))
    Printable.pprint("Cats are meeting here!")
    Printable.pprint(2)
    Printable.pprint(false)
    Printable.pprint(new Date)
    Printable.pprint(mizzi)
    Printable.pprint(garfield)
  }

  {
    println("\n--> This looks a bit nicer if we import the methods of the companion object ...")

    import Printable._

    pprint("Cats are meeting here!")
    pprint(2)
    pprint(false)
    pprint(new Date)
    pprint(mizzi)
    pprint(garfield)
  }

  {
    println("\n--> now using extension methods (type enrichment) ...")

    import Printable.syntax._

    "Cats are meeting here!".pprint
    2.pprint
    false.pprint
    new Date().pprint
    mizzi.pprint
    garfield.pprint
  }

  {
    println("\n--> stringifying/printing Option[A] ...")

    import Printable.syntax._

    Option("Cats are meeting here!").pprint
    Option.empty[String].pprint
    Option(2).pprint
    Option(false).pprint
    Option(new Date()).pprint
    Option(mizzi).pprint
    Option(garfield).pprint
    Option.empty[Cat].pprint
  }

  {
    println("\n--> stringifying/printing List[A] ...")

    import Printable.syntax._

    List("Cats", "are", "meeting", "here", "!").pprint
    List.empty[String].pprint
    List(2, 3, 4).pprint
    List(false, true, 2 == 3).pprint
    List(mizzi, garfield).pprint
    List.empty[Cat].pprint
  }

  println("-----\n")
}
