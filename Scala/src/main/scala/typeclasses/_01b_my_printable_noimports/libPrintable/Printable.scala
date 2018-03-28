package typeclasses._01b_my_printable_without_imports.libPrintable

import java.util.Date

import scala.language.implicitConversions

// the type class, a trait with at least one type parameter
//
trait Printable[A] {
  def format(value: A): String
}

// The type class companion object
//
object Printable {

  // interface object methods for the type class
  //
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))

  // the type class instances for standard types
  //
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = value.toString
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val datePrintable: Printable[Date] = new Printable[Date] {
    override def format(value: Date): String = value.toString
  }

  // interface syntax methods as extension methods
  // converts   A => Printable.Ops[A]
  //
  class Ops[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(format)
  }
}
