package typeclasses._01c_my_printable_noimport

import typeclasses._01c_my_printable_noimport.libPrintable.PrintableUtils

package object user extends PrintableUtils {

  // In the users package object base trait utilities and implicits can be overridden.

  // override def pprint[A: Printable](value: A): Unit = println("class: " + value.getClass.getSimpleName + " --- value: " + stringify(value))

  // implicit class MyPrintableOps[A: Printable](value: A) extends PrintableOps(value) {
  //   override def pprint: Unit = println("class: " + value.getClass.getSimpleName + " --- value: " + super.stringify)
  // }

  // implicit val pkgDatePrintable: Printable[Date] = (value: Date) => "THE PACKAGE DATE IS ==> " + value.toString
}
