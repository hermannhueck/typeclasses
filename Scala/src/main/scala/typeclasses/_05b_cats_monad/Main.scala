package typeclasses._05b_cats_monad

import cats.Monad
import typeclasses._05b_cats_monad.id.Identity

object Main extends App {

  {
    println

    val myId = Monad[Identity].pure(32)
    println(s"-- pure: $myId")

    val mappedId = Monad[Identity].map(myId)(_ + 10)
    println(s"-- map: $mappedId")

    val flatMappedId = Monad[Identity].flatMap(myId)(x => Identity(x + 10))
    println(s"-- flatMap: $flatMappedId")

    val flattenedId = Monad[Identity].flatten(Identity(myId))
    println(s"-- flatten: $flattenedId")
  }

  {
    println

    import cats.instances.option._

    val myOpt = Option(32)
    println(s"-- Option(32): $myOpt")
    val myNone = Option.empty[Int]
    println(s"-- Option.empty: $myNone")

    val mappedOpt = Monad[Option].map(myOpt)(_ + 10)
    println(s"-- map: $mappedOpt")
    val mappedNone = Monad[Option].map(myNone)(_ + 10)
    println(s"-- map $mappedNone")

    val flatMappedOpt = Monad[Option].flatMap(myOpt)(x => Option(x + 10))
    println(s"-- flatMap: $flatMappedOpt")
    val flatMappedNone = Monad[Option].flatMap(myNone)(x => Option(x + 10))
    println(s"-- flatMap $flatMappedNone")

    val flattenedOpt = Monad[Option].flatten(myOpt.map(Option(_)))
    println(s"-- flatten: $flattenedOpt")
    val flattenedNone = Monad[Option].flatten(myNone.map(Option(_)))
    println(s"-- flatten: $flattenedNone")
  }

  {
    println

    import cats.instances.list._

    val myList = List(32, 33, 34)
    println(s"-- List(32, 33, 34): $myList")

    val mappedList = Monad[List].map(myList)(_ + 10)
    println(s"-- map: $mappedList")

    val flatMappedList = Monad[List].flatMap(myList)(x => List(x + 10))
    println(s"-- flatMap: $flatMappedList")

    val flattenedList = Monad[List].flatten(myList.map(List(_)))
    println(s"-- flatten: $flattenedList")
  }

  {
    println

    import typeclasses._05b_cats_monad.id.Identity.identityMonad
    import cats.syntax.applicative._

    val myId = 32.pure[Identity]
    println(s"-- pure: $myId")

    val mappedId = myId.map(_ + 10)
    println(s"-- map: $mappedId")

    val flatMappedId = myId.flatMap(x => (x + 10).pure[Identity])
    println(s"-- flatMap: $flatMappedId")

    val flattenedId = myId.map(_.pure[Identity]).flatten
    println(s"-- flatten: $flattenedId")
  }

  {
    println

    import cats.instances.option._
    import cats.syntax.applicative._

    val myOpt = 32.pure[Option]
    println(s"-- pure: $myOpt")
    val myNone = Option.empty[Int]
    println(s"-- Option.empty: $myNone")

    val mappedOpt = myOpt.map(_ + 10)
    println(s"-- map: $mappedOpt")
    val mappedNone = myNone.map(_ + 10)
    println(s"-- map $mappedNone")

    val flatMappedOpt = myOpt.flatMap(x => (x + 10).pure[Option])
    println(s"-- flatMap: $flatMappedOpt")
    val flatMappedNone = myNone.flatMap(x => (x + 10).pure[Option])
    println(s"-- flatMap $flatMappedNone")

    val flattenedOpt = myOpt.map(_.pure[Option]).flatten
    println(s"-- flatten: $flattenedOpt")
    val flattenedNone = myNone.map(_.pure[Option]).flatten
    println(s"-- flatten: $flattenedNone")
  }

  {
    println

    import cats.instances.list._
    import cats.syntax.applicative._

    val myList = List(32, 33, 34)
    println(s"-- List(32, 33, 34): $myList")

    val mappedList = myList.map(_ + 10)
    println(s"-- map: $mappedList")

    val flatMappedList = myList.flatMap(x => (x + 10).pure[List])
    println(s"-- flatMap: $flatMappedList")

    val flattenedList = myList.map(_.pure[List]).flatten
    println(s"-- flatten: $flattenedList")
  }

  println
}
