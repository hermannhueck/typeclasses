package typeclasses._01c_my_printable_oneimport.domain

import typeclasses._01c_my_printable_oneimport.libPrintable._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def stringify(cat: Cat): String = {
      val name  = cat.name.stringify
      val age   = cat.age.stringify
      val color = cat.color.stringify
      s"$name is a $age year-old $color cat."
    }
  }
}
