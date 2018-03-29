package typeclasses._01b_my_printable_oneimport

package object libPrintable {

  // interface object methods for the type class
  // here using a context bound + implicitly instead of an implicit parameter
  //
  def stringify[A: Printable](value: A): String = Printable[A].stringify(value) // same as:

  // def stringify[A: Printable](value: A): String = implicitly[Printable[A]].stringify(value)
  def pprint[A: Printable](value: A): Unit = println(stringify(value))

  // def stringify[A](value: A)(implicit p: Printable[A]): String = p.stringify(value)
  // def pprint[A](value: A)(implicit p: Printable[A]): Unit = println(stringify(value))

  // implicit class (with context bound and implicitly)
  // provides interface syntax as extension methods
  // converts   A => PrintableOps[A]
  //
  implicit class PrintableOps[A: Printable](value: A) {
    def stringify: String = implicitly[Printable[A]].stringify(value)
    def pprint(): Unit = println(stringify)
  }
}
