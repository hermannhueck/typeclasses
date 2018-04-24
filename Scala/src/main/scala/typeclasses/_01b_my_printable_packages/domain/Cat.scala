package typeclasses._01b_my_printable_packages.domain

import typeclasses._01b_my_printable_packages.libPrintable.Printable
import typeclasses._01b_my_printable_packages.libPrintable.Printable.instances._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def stringify(cat: Cat): String = {
      val name  = Printable.stringify(cat.name)
      val age   = Printable.stringify(cat.age)
      val color = Printable.stringify(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }
}
