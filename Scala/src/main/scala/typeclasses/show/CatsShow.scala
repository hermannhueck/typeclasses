package typeclasses.show

object CatsShow extends App {

  import cats.Show

  // -------------------------
  println("\n--> using Show.apply ...\n")

  import cats.instances.int._

  val showInt:    Show[Int]    = Show.apply[Int]
  val intAsString: String = showInt.show(123)
  println(intAsString)

  import cats.instances.string._

  val showString: Show[String] = Show[String]
  val stringAsString: String = showString.show("abc")
  println(stringAsString)

  import cats.instances.double._

  println(Show[Double].show(1.5))

  // -------------------------
  println("\n--> using interface syntax ...\n") // for show

  import cats.syntax.show._

  val shownInt = 123.show
  val shownString = "abc".show

  println(shownInt)
  println(shownString)
  println(123.show)
  println("abc".show)

  // -------------------------
  println("\n--> using show for Option ...\n")

  import cats.instances.option._

  println(Option("abc").show)
  // println(Some("abc").show)   // this doesn't work
  val opt: Option[String] = Some("abc") // this works!
  println(opt.show)
  val opt2: Option[String] = None // this works only if the type of opt2 is specified
  println(opt2.show)
  println(Option.empty[String].show)

  // -------------------------
  println("\n--> using show for java.util.Date ...\n")

  import java.util.Date

  showDate1()
  showDate2()
  showDate3()

  private def showDate1(): Unit = {
    implicit val dateShow: Show[Date] =
      new Show[Date] {
        def show(date: Date): String =
          s"${date.getTime} ms since the epoch."
      }
    println(new Date().show)
  }

  private def showDate2(): Unit = {
    // same as showDate1 using single abstract method
    implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime} ms since the epoch.")
    println(new Date().show)
  }

  private def showDate3(): Unit = {
    implicit val dateShow: Show[Date] = Show.fromToString
    println(new Date().show)
  }

  // -------------------------
  println("\n--> using show for Cat ...\n")

  import typeclasses.show.domain.Cat

  val mizzi = Cat("Mizzi", 1, "black and white")
  val garfield = Cat("Garfield", 38, "ginger and black")

  println(mizzi.show)
  println(garfield.show)

  println
}
