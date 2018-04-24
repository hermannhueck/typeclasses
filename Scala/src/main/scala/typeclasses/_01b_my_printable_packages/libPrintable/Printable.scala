package typeclasses._01b_my_printable_packages.libPrintable

import java.util.Date

// the type class, a trait with at least one type parameter
//
trait Printable[A] {
  def stringify(value: A): String
  def pprint[B <: A](value: B): Unit = println(stringify(value))
}

// The type class companion object
//
object Printable {

  // interface object methods for the type class
  //
  def stringify[A](value: A)(implicit p: Printable[A]): String = p.stringify(value)
  def pprint[A](value: A)(implicit p: Printable[A]): Unit = println(stringify(value))

  // the type class instances for standard types
  //
  object instances {

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


    // generic instances

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
  }

  // interface syntax methods as extension methods
  //
  object syntax {

    implicit class PrintableOps[A](value: A) {
      def stringify(implicit p: Printable[A]): String = p.stringify(value)
      def pprint(implicit p: Printable[A]): Unit = println(stringify)
    }
  }
}
