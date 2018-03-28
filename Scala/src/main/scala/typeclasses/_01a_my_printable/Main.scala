package typeclasses._01a_my_printable

import java.util.Date

import typeclasses._01a_my_printable.domain.Cat
import typeclasses._01a_my_printable.lib.Printable
import Printable.instances._

object Main extends App {

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    // overrides PrintableInstances.intPrintable which is in scope but has lower priority
    implicit val intPrintable: Printable[Int] = new Printable[Int] {
      override def format(value: Int): String = "How many cats? " + value.toString
    }

    // overrides PrintableInstances.datePrintable which is in scope but has lower priority
    implicit val datePrintable: Printable[Date] = new Printable[Date] {
      override def format(value: Date): String = "Date of meeting: " + value.toString
    }

    def myPrint[A](value: A)(implicit printable: Printable[A]): Unit =
      println(printable.format(value))

    println("\n-----")

    println("---> Using the local method 'myPrint' and local Printable instances to print ...")

    myPrint("Cats are meeting here!")
    myPrint(2)
    myPrint(new Date)
    myPrint(mizzi)
    myPrint(garfield)
  }

  {
    println("---> Using the Printable companion object to format and print ...")

    println(Printable.format("Cats are meeting here!"))
    Printable.print("Cats are meeting here!")
    Printable.print(2)
    Printable.print(new Date)
    Printable.print(mizzi)
    Printable.print(garfield)
  }

  {
    println("\n--> This looks a bit nicer if we import the methods of the companion object ...")

    import Printable._

    print("Cats are meeting here!")
    print(2)
    print(new Date)
    print(mizzi)
    print(garfield)
  }

  {
    println("\n--> now using extension methods (type enrichment) ...")

    import Printable.syntax._

    "Cats are meeting here!".print
    2.print
    new Date().print
    mizzi.print
    garfield.print
  }

  println("-----\n")
}