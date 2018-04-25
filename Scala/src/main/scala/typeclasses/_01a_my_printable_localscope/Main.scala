package typeclasses._01a_my_printable_localscope

import java.util.Date

trait Printable[A] {
  def stringify(value: A): String
  def pprint(value: A): Unit = println(stringify(value))
}


final case class Cat(name: String, age: Int, color: String)


object Main extends App {

  // interface object methods for the type class
  //
  def stringify[A](value: A)(implicit p: Printable[A]): String = p.stringify(value)
  def pprint[A](value: A)(implicit p: Printable[A]): Unit = p.pprint(value)


  // the type class instances for standard types
  //
  // Inside the type classes companion object the instances are in implicit scope
  // and hence looked up automatically by the compliler (withour needing an extra import).
  //
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def stringify(value: String): String = value
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def stringify(value: Boolean): String = value.toString
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def stringify(value: Int): String = value.toString
  }

  implicit val datePrintable: Printable[Date] = new Printable[Date] {
    override def stringify(value: Date): String = value.toString
  }

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def stringify(cat: Cat): String =
      s"${cat.name} is a ${cat.age.toString} year-old ${cat.color} cat."
  }


  // generic instances defined as defs, not vals

  // if you can stringify an A, you can also stringify Option[A]
  //
  implicit def optionPrintable[A](implicit pA: Printable[A]): Printable[Option[A]] = new Printable[Option[A]] {
    override def stringify(maybeA: Option[A]): String =
      maybeA.map(pA.stringify)
        .map(s => s"Option($s)")
        .getOrElse("None")
  }

  // if you can stringify an A, you can also stringify List[A]
  //
  implicit def listPrintable[A](implicit lA: Printable[A]): Printable[List[A]] = new Printable[List[A]] {
    override def stringify(as: List[A]): String =
      as.map(lA.stringify)
        .mkString("List(", ", ", ")")
  }


  // interface syntax methods as extension methods
  //
  implicit class PrintableOps[A](value: A) {
    def stringify(implicit p: Printable[A]): String = p.stringify(value)
    def pprint(implicit p: Printable[A]): Unit = println(stringify)
  }



  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  {
    println("\n--> This looks a bit nicer if we import the methods of the companion object ...")

    println(stringify("Cats are meeting here!"))
    pprint("Cats are meeting here!")
    pprint(2)
    pprint(false)
    pprint(new Date)

    pprint(mizzi)
    pprint(garfield)
    pprint(Option(garfield))
    pprint(List(mizzi, garfield))
  }

  {
    println("\n--> now using extension methods (type enrichment) ...")

    println("Cats are meeting here!".stringify)
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
    Option.empty[String].pprint
    Option(false).pprint
    Option(new Date()).pprint
    Option.empty[Date].pprint

    Option(mizzi).pprint
    Option(garfield).pprint
    Option.empty[Cat].pprint
  }

  {
    println("\n--> stringifying/printing List[A] ...")

    List("Cats", "are", "meeting", "here", "!").pprint
    List.empty[String].pprint
    List(2, 3, 4).pprint
    List(false, true, 2 == 3).pprint

    List(mizzi, garfield).pprint
    List.empty[Cat].pprint
  }

  println("-----\n")
}
