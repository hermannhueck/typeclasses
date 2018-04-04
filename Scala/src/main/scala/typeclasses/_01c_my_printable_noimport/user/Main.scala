package typeclasses._01c_my_printable_noimport.user

import java.util.Date

import typeclasses._01c_my_printable_noimport.domain.Cat

// No import needed to use libPrintable - see package object

object Main extends App {

  // this local instance Printable[Date] conflicts with the package instance if defined
  // implicit val localDatePrintable: Printable[Date] = (value: Date) => "THE LOCAL DATE IS ==> " + value.toString

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    println("\n--> Interface object methods ...")

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
    println("\n--> now using our pimp (= extension methods, type enrichment) ...")

    "Cats are meeting here!".pprint
    2.pprint
    false.pprint
    new Date().pprint
    val stringifiedMizzi = mizzi.stringify
    println(stringifiedMizzi)
    mizzi.pprint
    garfield.pprint
  }

  {
    println("\n--> stringifying/pprinting Option[A] ...")

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
    println("\n--> stringifying/pprinting List[A] ...")

    List("Cats", "are", "meeting", "here", "!").pprint
    List.empty[String].pprint
    List(2, 3, 4).pprint
    List(false, true, 2 == 3).pprint
    List(mizzi, garfield).pprint
    List.empty[Cat].pprint
  }

  println("-----\n")
}
