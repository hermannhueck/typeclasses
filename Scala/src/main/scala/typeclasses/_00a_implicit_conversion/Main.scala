package typeclasses._00a_implicit_conversion

object Main extends App {

  println

  import scala.language.implicitConversions

  implicit def string2int(s: String): Int = Integer.parseInt(s)

  val x: Int = "5" // !! Assign a string to an int val

  def useInt(x: Int): Unit = println("Int value: " + x)
  useInt("42") // !! Pass a string to an int param

  println
}
