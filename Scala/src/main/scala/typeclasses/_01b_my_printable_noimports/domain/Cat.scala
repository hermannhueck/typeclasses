package typeclasses._01b_my_printable_without_imports.domain

import typeclasses._01b_my_printable_without_imports.libPrintable._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name  = cat.name.format
      val age   = cat.age.format
      val color = cat.color.format
      s"$name is a $age year-old $color cat."
    }
  }
}
