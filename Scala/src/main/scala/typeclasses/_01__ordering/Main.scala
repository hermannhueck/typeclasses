package typeclasses._01__ordering

import typeclasses._01__ordering.domain.Cat
import typeclasses._01__ordering.domain.CatOrderings

object Main extends App {

  val ints: List[Int] = List(6, 18, 3, 2, 12, 13, 1)

  val cats: List[Cat] = List(
    Cat("Garfield", 38, "ginger and black"),
    Cat("Mizzi", 1, "white"),
    Cat("Mimi", 1, "white"),
    Cat("Carlo", 12, "black and white"),
    Cat("Tiger", 1, "striped"),
  )

  println("-----")

  println("\n-- Ints unorderd:")
  println(ints)

  println("\n-- Ints with default ordering (ascending):")
  println(ints.sorted)

  println("\n-- Ints in descending order:")
  implicit object intOrderingDesc extends Ordering[Int] {
    override def compare(x: Int, y: Int): Int =
      if (y < x) -1 else if (y > x) 1 else 0 // descending order
  }
  println(ints.sorted)

  println("\n-----")

  println("\n-- Cats unorderd:")
  println(cats)

  println("\n-- Cats with default ordering (by age, color, name):")
  println(cats.sorted)

  // The following code blocks restricts the visibility of the
  // imported implicit type class instances to the respective block.
  // Otherwise the compiler would bail out with an ambiguity error.

  {
    println("\n-- Cats by name ascending:")
    import CatOrderings.catOrderingByNameAsc
    println(cats.sorted)
  }

  {
    println("\n-- Cats by name descending:")
    import CatOrderings.catOrderingByNameDesc
    println(cats.sorted)
  }

  {
    println("\n-- Cats by age ascending:")
    import CatOrderings.catOrderingByAgeAsc
    println(cats.sorted)
  }

  {
    println("\n-- Cats by age descending:")
    import CatOrderings.catOrderingByAgeDesc
    println(cats.sorted)
  }

  {
    println("\n-- Cats by color ascending:")
    import CatOrderings.catOrderingByColorAsc
    println(cats.sorted)
  }

  {
    println("\n-- Cats by color descending:")
    import CatOrderings.catOrderingByColorDesc
    println(cats.sorted)
  }

  println("\n-----")
}
