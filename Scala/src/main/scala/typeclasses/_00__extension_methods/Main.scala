package typeclasses._00__extension_methods

object Main extends App {

  println("\n----- Pimpin' type Int ...")

  implicit class EnrichedInt(i: Int) {
    def double: Int = 2 * i
    def triple: Int = 3 * i
    def square: Int = i * i
    def cube: Int = i * i * i
  }

  val double5: Int = 5.double
  val triple5: Int = 5.triple
  val squared5: Int = 5.square
  val cubed5: Int = 5.cube
  val doubledSquared5 = 5.double.square
  
  println(s"5.double = $double5")
  println(s"5.triple = $triple5")
  println(s"5.square = $squared5")
  println(s"5.cube = $cubed5")
  println(s"5.doubledSquared5 = $doubledSquared5")


  println("\n----- Pimpin' type Cat ...")

  final case class Cat(name: String, age: Int, color: String)

  implicit class PimpedCat(c: Cat) {
    def description: String = s"${c.name} is a ${c.age} year old ${c.color} colored cat."
    def describe(): Unit = println(c.description)
  }

  val mizzi = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  print("mizzi.describe() -->  ")
  mizzi.describe()
  print("garfield.describe() -->  ")
  garfield.describe()


  println("\n----- Pimpin' type List ...")

  implicit class PimpedList[A](xs: List[A]) {

    // Haskell's list provides zipWith, Scala lists don't.
    def zipWith[B, C](ys: List[B])(f: (A, B) => C): List[C] =
      xs zip ys map { case (x, y) => f(x, y) }
  }

  val l1 = List(1, 2, 3)
  val l2 = List(10, 20, 30)

  val result = l1.zipWith(l2)(_ + _)
  println(result) // --> List(11, 22, 33)

  println
}
