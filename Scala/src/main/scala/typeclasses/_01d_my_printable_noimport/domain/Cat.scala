package typeclasses._01d_my_printable_noimport.domain

import typeclasses._01d_my_printable_noimport.libPrintable._

case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catPrintable: Printable[Cat] = (cat: Cat) => {
    val name = cat.name.stringify
    val age = cat.age.stringify
    val color = cat.color.stringify
    s"$name is a $age year-old $color cat."
  }
}
