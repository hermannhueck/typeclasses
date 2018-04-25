package typeclasses._03d_my_printable_noimport.libPrintable

import java.util.Date

import scala.language.implicitConversions

// type class, a trait with at least one type parameter
trait Printable[A] {
  def stringify(value: A): String
  def pprint(value: A): Unit = println(stringify(value))
}

// type class companion object
object Printable {

  // --- apply method for convenience
  def apply[A: Printable]: Printable[A] = implicitly[Printable[A]] // same as:
  // def apply[A](implicit printable: Printable[A]): Printable[A] = printable

  // --- the type class instances for standard types
  implicit val stringPrintable: Printable[String] = (value: String) => value
  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString
  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => value.toString
  implicit val datePrintable: Printable[Date] = (value: Date) => value.toString

  implicit def optionPrintable[A: Printable]: Printable[Option[A]] =
    (optA: Option[A]) => optA
      .map(Printable[A].stringify)
      .map(s => s"Option($s)")
      .getOrElse("None")

  implicit def listPrintable[A: Printable]: Printable[List[A]] =
    (listA: List[A]) => listA
      .map(Printable[A].stringify)
      .mkString("List(", ", ", ")")
}

// trait written to be extended by package object:
// either library package object or user package object
//
trait PrintableUtils {

  // interface object methods
  def stringify[A: Printable](value: A): String = Printable[A].stringify(value)
  def pprint[A: Printable](value: A): Unit = Printable[A].pprint(value)

  // pimp = type enrichment = extension methods
  implicit class PrintableOps[A: Printable](value: A) {
    def stringify: String = Printable[A].stringify(value)
    def pprint: Unit = Printable[A].pprint(value)
  }

  // implicit def toPrintableOps[A: Printable](value: A): PrintableOps[A] = new PrintableOps[A](value)
}
