package typeclasses._03c_my_printable_oneimport

package object libPrintable {

  // interface object methods for the type class
  // here using a context bound + implicitly instead of an implicit parameter
  //
  def stringify[A: Printable](value: A): String = Printable[A].stringify(value) // same as:
  def pprint[A: Printable](value: A): Unit = Printable[A].pprint(value)

  // def stringify[A: Printable](value: A): String = implicitly[Printable[A]].stringify(value)
  // def pprint[A: Printable](value: A): Unit = implicitly[Printable[A]].pprint(value)

  // def stringify[A](value: A)(implicit p: Printable[A]): String = p.stringify(value)
  // def pprint[A](value: A)(implicit p: Printable[A]): Unit = p.pprint(value)


  // 4 ways to provide interface syntax as extension methods
  // The implicit class converts   A => PrintableOps[A]

  // 1. implicit class (with implicit method parameters)
  //
  /*
  implicit class PrintableOps[A](value: A) {
    def stringify(implicit p: Printable[A]): String = p.stringify(value)
    def pprint(implicit p: Printable[A]): Unit = p.pprint(value)
  }
  */

  // 2. implicit class (with context bound and implicitly)
  //
  /*
  implicit class PrintableOps[A: Printable](value: A) {
    def stringify: String = implicitly[Printable[A]].stringify(value)
    def pprint(): Unit = implicitly[Printable[A]].pprint(value)
  }
  */

  // 3. implicit class (with context bound and apply)
  //
  implicit class PrintableOps[A: Printable](value: A) {
    def stringify: String = Printable[A].stringify(value)
    def pprint(): Unit = Printable[A].pprint(value)
  }
  /*
  */

  // 4. Using an implicit conversion instead of an implicit class
  //
  /*
  import scala.language.implicitConversions // discouraged

  implicit def convertToPrintableOps[A](value: A) = new {
    def stringify(implicit p: Printable[A]): String = p.stringify(value)
    def pprint(implicit p: Printable[A]): Unit = println(stringify)
  }
  */
}
