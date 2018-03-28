package typeclasses._01b_my_printable_noimports

package object libPrintable {

  // suppress compiler warning about implicit conversion
  import scala.language.implicitConversions

  // implicit conversion method: converts   A => Printable.Ops[A]
  implicit def syntax[A](a: A): Printable.Ops[A] = new Printable.Ops[A](a)
}
