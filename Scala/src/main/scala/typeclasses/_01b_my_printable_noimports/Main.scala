package typeclasses._01b_my_printable_without_imports

import java.util.Date

import typeclasses._01b_my_printable_without_imports.domain.Cat
import typeclasses._01b_my_printable_without_imports.libPrintable._

object Main extends App {

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    println("\n--> This looks a bit nicer if we import the methods of the companion object ...")

    print("Cats are meeting here!")
    print(2)
    print(new Date)
    print(mizzi)
    print(garfield)
  }

  {
    println("\n--> now using extension methods (type enrichment) ...")

    "Cats are meeting here!".print
    2.print
    new Date().print
    mizzi.print
    garfield.print
  }

  println("-----\n")
}
