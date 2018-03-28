package typeclasses._01c_cats_show.domain

import cats.Show
import cats.instances.string._
import cats.instances.int._
import cats.syntax.show._

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catShow: Show[Cat] = Show.show { cat =>
    val name  = cat.name.show
    val age   = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}
