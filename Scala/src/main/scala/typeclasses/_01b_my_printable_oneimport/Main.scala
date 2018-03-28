package typeclasses._01b_my_printable_oneimport

import java.util.Date

import typeclasses._01b_my_printable_oneimport.domain._
import typeclasses._01b_my_printable_oneimport.libPrintable._

object Main extends App {

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    println("\n--> This looks a bit nicer if we import the methods of the companion object ...")

    import Printable.print   // Without this import the standard print method is preferred.

    print("Cats are meeting here!")
    print(2)
    print(new Date)
    print(mizzi)
    print(garfield)
  }

  {
    println("\n--> now using extension methods (type enrichment) ...")

    "Cats are meeting here!".print()
    2.print()
    new Date().print()
    mizzi.print()
    garfield.print()
  }

  {
    println("\n--> stringifying/printing Option[A] ...")

    Option("Cats are meeting here!").print()
    Option.empty[String].print()
    Option(2).print()
    Option(new Date()).print()
    Option(mizzi).print()
    Option(garfield).print()
    Option.empty[Cat].print()
  }

  println("-----\n")
}
