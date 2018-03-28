package typeclasses._01b_my_printable_oneimport

package object libPrintable {

  // implicit class (with context bound and implicitly)
  // provides interface syntax as extension methods
  // converts   A => PrintableOps[A]
  //
  implicit class PrintableOps[A: Printable](value: A) {
    def stringify: String = implicitly[Printable[A]].stringify(value)
    def print(): Unit = println(stringify)
  }
}
