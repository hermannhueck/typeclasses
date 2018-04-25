package typeclasses._07__ordering

import typeclasses._07__ordering.domain.Cat
import typeclasses._07__ordering.domain.CatOrderings

object Main extends App {

  val cats = List(
    Cat("Garfield", 38, "ginger and black"),
    Cat("Mizzi", 1, "white"),
    Cat("Mimi", 1, "white"),
    Cat("Carlo", 12, "black and white"),
    Cat("Tiger", 1, "striped"),
  )

  println("-----")

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
