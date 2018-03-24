package typeclasses._01a_my_printable.domain

import typeclasses._01a_my_printable.lib.Printable
import typeclasses._01a_my_printable.lib.Printable.instances._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name  = Printable.format(cat.name)
      val age   = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }
}
