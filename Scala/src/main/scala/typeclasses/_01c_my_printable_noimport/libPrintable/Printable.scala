package typeclasses._01c_my_printable_noimport.libPrintable

import java.util.Date

import scala.language.implicitConversions

// type class, a trait with at least one type parameter
trait Printable[A] {
  def stringify(value: A): String
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
      .map(Printable[A].stringify) // same as:
      .map(s => s"Option($s)")
      .getOrElse("None")

  implicit def listPrintable[A: Printable]: Printable[List[A]] =
    (listA: List[A]) => listA
      .map(Printable[A].stringify)
      .mkString("List(", ", ", ")")
}

trait PrintableUtils {

  // interface object methods
  def stringify[A: Printable](value: A): String = Printable[A].stringify(value) // same as:
  def pprint[A: Printable](value: A): Unit = println(stringify(value))


  // pimp = type enrichment = extension methods
  implicit class PrintableOps[A: Printable](value: A) {
    def stringify: String = Printable[A].stringify(value)
    def pprint: Unit = println(stringify)
  }
}
