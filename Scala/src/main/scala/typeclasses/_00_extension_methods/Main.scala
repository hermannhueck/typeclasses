package typeclasses._00_extension_methods

object Main extends App {

  println("\n----- Type enrichment for type Int ...")

  implicit class IntExtensions(i: Int) {
    def double: Int = 2 * i
    def triple: Int = 3 * i
    def square: Int = i * i
    def cube: Int = i * i * i
  }

  val double5: Int = 5.double
  val triple5: Int = 5.triple
  val squared5: Int = 5.square
  val cubed5: Int = 5.cube
  
  println(s"5.double = ${double5}")
  println(s"5.triple = ${triple5}")
  println(s"5.square = ${squared5}")
  println(s"5.cube = ${cubed5}")


  println("\n----- Type enrichment for type Cat ...")

  final case class Cat(name: String, age: Int, color: String)

  implicit class CatExtensions(c: Cat) {
    def description: String = s"${c.name} is a ${c.age} year old ${c.color} colored cat."
    def describe(): Unit = println(c.description)
  }

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  print("mizzi.describe() -->  ")
  mizzi.describe()
  print("garfield.describe() -->  ")
  garfield.describe()

  println
}
