package typeclasses._01b_my_printable_oneimport.libPrintable

import java.util.Date

import scala.annotation.{implicitAmbiguous, implicitNotFound}
import scala.language.implicitConversions

// the type class, a trait with at least one type parameter
//
@implicitNotFound("No instance of type class Printable found for type ${A}")
@implicitAmbiguous("More than one instance of type class Printable found for type ${A}")
trait Printable[A] {
  def stringify(value: A): String
}

// The type class companion object
//
object Printable {

  // def apply[A: Printable]: Printable[A] = implicitly[Printable[A]] // same as:
  // def apply[A](implicit printable: Printable[A]): Printable[A] = printable

  // interface object methods for the type class
  // here using a context bound + implicitly instead of an implicit parameter
  //
  // def stringify[A: Printable](value: A): String = Printable[A].stringify(value) // same as:
  // def stringify[A: Printable](value: A): String = implicitly[Printable[A]].stringify(value)
  // def print[A: Printable](value: A): Unit = println(stringify(value))
  def stringify[A](value: A)(implicit p: Printable[A]): String = p.stringify(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(stringify(value))

  // the type class instances for standard types
  //
  implicit val stringPrintable: Printable[String] = (value: String) => value

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => value.toString

  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString

  implicit val datePrintable: Printable[Date] = (value: Date) => value.toString

  // a generic instance for Option[A] is a def with a type
  // parameter A and an implicit Printable[A]. That means:
  // if you can stringify an A, you also can stringify Option[A]
  //
  implicit def optionPrintable[A: Printable]: Printable[Option[A]] =
    (optA: Option[A]) => optA
      .map(implicitly[Printable[A]].stringify)
      .map(s => s"Option($s)")
      .getOrElse("None")
}
