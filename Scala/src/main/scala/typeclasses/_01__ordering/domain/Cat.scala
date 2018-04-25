package typeclasses._01__ordering.domain

final case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catDefaultOrdering: Ordering[Cat] = new Ordering[Cat] {
    // Cats default ordering:
    // 1. by age ascending
    // 2. by name ascending
    // 3. by color ascending
    override def compare(x: Cat, y: Cat): Int = {
      val byAge = Ordering[Int].compare(x.age, y.age)
      if (byAge != 0)
        byAge
      else {
        val byColor = Ordering[String].compare(x.color, y.color)
        if (byColor != 0)
          byColor
        else
          Ordering[String].compare(x.name, y.name)
      }
    }
  }
}

object CatOrderings {

  implicit val catOrderingByNameAsc: Ordering[Cat] = new Ordering[Cat] {
    override def compare(x: Cat, y: Cat): Int = Ordering[String].compare(x.name, y.name)
  }

  implicit val catOrderingByNameDesc: Ordering[Cat] = new Ordering[Cat] {
    override def compare(x: Cat, y: Cat): Int = Ordering[String].compare(y.name, x.name)
  }

  implicit val catOrderingByAgeAsc: Ordering[Cat] = new Ordering[Cat] {
    override def compare(x: Cat, y: Cat): Int = Ordering[Int].compare(x.age, y.age)
  }

  implicit val catOrderingByAgeDesc: Ordering[Cat] = new Ordering[Cat] {
    override def compare(x: Cat, y: Cat): Int = Ordering[Int].compare(y.age, x.age)
  }

  implicit val catOrderingByColorAsc: Ordering[Cat] = new Ordering[Cat] {
    override def compare(x: Cat, y: Cat): Int = Ordering[String].compare(x.color, y.color)
  }

  implicit val catOrderingByColorDesc: Ordering[Cat] = new Ordering[Cat] {
    override def compare(x: Cat, y: Cat): Int = Ordering[String].compare(y.color, x.color)
  }
}
