package typeclasses._03c_my_printable_oneimport.libPrintable

import java.util.Date

import scala.annotation.{implicitAmbiguous, implicitNotFound}
import scala.language.implicitConversions

// the type class, a trait with at least one type parameter
//
@implicitNotFound("No instance of type class Printable found for type ${A}")
@implicitAmbiguous("More than one instance of type class Printable found for type ${A}")
trait Printable[A] {
  def stringify(value: A): String
  def pprint(value: A): Unit = println(stringify(value))
}

// The type class companion object
//
object Printable {

  // Printable.apply[A] or Printable[A]
  // makes the implicit Printable[A] explicitly available
  //
  def apply[A: Printable]: Printable[A] = implicitly[Printable[A]] // same as:
  // def apply[A](implicit printable: Printable[A]): Printable[A] = printable


  // the type class instances for standard types
  // implicit vals, defs or objects can be used.
  //
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def stringify(value: String): String = value
  }

  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString

  implicit object booleanPrintable extends Printable[Boolean] {
    override def stringify(value: Boolean): String = value.toString
  }

  implicit val datePrintable: Printable[Date] = (value: Date) => value.toString


  // generic instances

  // if you can stringify an A, you can also stringify Option[A]
  //
  implicit def optionPrintable[A: Printable]: Printable[Option[A]] =
    (optA: Option[A]) => optA
      .map(Printable[A].stringify) // same as:
      // .map(implicitly[Printable[A]].stringify)
      .map(s => s"Option($s)")
      .getOrElse("None")

  // if you can stringify an A, you can also stringify List[A]
  //
  implicit def listPrintable[A: Printable]: Printable[List[A]] =
    (as: List[A]) => as
      .map(Printable[A].stringify)
      .mkString("List(", ", ", ")")
}
