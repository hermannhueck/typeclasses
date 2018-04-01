package typeclasses._01b_my_printable_oneimport

import java.util.Date

import typeclasses._01b_my_printable_oneimport.domain._

// This is the only (wildcard) import needed to use libPrintable
//
import typeclasses._01b_my_printable_oneimport.libPrintable._

object Main extends App {

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    println("\n--> This looks a bit nicer if we import the methods of the companion object ...")

    pprint("Cats are meeting here!")
    pprint(2)
    pprint(false)
    pprint(new Date)
    pprint(mizzi)
    pprint(garfield)
    println(stringify(garfield))
  }

  // import scala.language.reflectiveCalls

  {
    println("\n--> now using extension methods (type enrichment) ...")

    "Cats are meeting here!".pprint
    2.pprint
    false.pprint
    new Date().pprint
    mizzi.pprint
    garfield.pprint
  }

  {
    println("\n--> stringifying/printing Option[A] ...")

    Option("Cats are meeting here!").pprint
    Option.empty[String].pprint
    Option(2).pprint
    Option(false).pprint
    Option(new Date()).pprint
    Option(mizzi).pprint
    Option(garfield).pprint
    Option.empty[Cat].pprint
  }

  println("-----\n")
}
