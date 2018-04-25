package typeclasses._07b_cats_monad

import cats.Monad
import typeclasses._07b_cats_monad.id.Identity

object Main extends App {

  println
  println("----- Using Monad[A].apply")

  {
    println

    val myId = Monad[Identity].pure(32) // or: Identity(32)
    println(s"-- Monad[Identity].pure: $myId")

    val mappedId = Monad[Identity].map(myId)(_ + 10)
    println(s"-- Monad[Identity].map: $mappedId")

    val flatMappedId = Monad[Identity].flatMap(myId)(x => Identity(x + 10))
    println(s"-- Monad[Identity].flatMap: $flatMappedId")

    val unFlattenedId: Identity[Identity[Int]] = Identity(myId)
    val flattenedId = Monad[Identity].flatten(unFlattenedId)
    println(s"-- Monad[Identity].flatten: $flattenedId")
  }

  {
    println

    import cats.instances.option._

    val myOpt = Monad[Option].pure(32)
    println(s"-- Monad[Option].pure: $myOpt")
    val myNone = Option.empty[Int]
    println(s"-- Option.empty: $myNone")

    val mappedOpt = Monad[Option].map(myOpt)(_ + 10)
    println(s"-- Monad[Option].map: $mappedOpt")
    val mappedNone = Monad[Option].map(myNone)(_ + 10)
    println(s"-- Monad[Option].map $mappedNone")

    val flatMappedOpt = Monad[Option].flatMap(myOpt)(x => Option(x + 10))
    println(s"-- Monad[Option].flatMap: $flatMappedOpt")
    val flatMappedNone = Monad[Option].flatMap(myNone)(x => Option(x + 10))
    println(s"-- Monad[Option].flatMap $flatMappedNone")

    val unFlattenedOpt: Option[Option[Int]] = myOpt.map(Option(_))
    val flattenedOpt = Monad[Option].flatten(unFlattenedOpt)
    println(s"-- Monad[Option].flatten: $flattenedOpt")
    val unFlattenedNone: Option[Option[Int]] = myNone.map(Option(_))
    val flattenedNone = Monad[Option].flatten(unFlattenedNone)
    println(s"-- Monad[Option].flatten: $flattenedNone")
  }

  {
    println

    import cats.instances.list._

    val myList = List(32, 33, 34)
    println(s"-- List.apply: $myList")

    val mappedList = Monad[List].map(myList)(_ + 10)
    println(s"-- Monad[List].map: $mappedList")

    val flatMappedList = Monad[List].flatMap(myList)(x => List(x + 10))
    println(s"-- Monad[List].flatMap: $flatMappedList")

    val unFlattenedList: List[List[Int]] = myList.map(List(_))
    val flattenedList = Monad[List].flatten(unFlattenedList)
    println(s"-- Monad[List].flatten: $flattenedList")
  }

  println
  println("----- Using Monad.syntax")

  {
    println

    import Identity.identityMonad
    import cats.syntax.applicative._

    val myId = 32.pure[Identity]
    println(s"-- Applicative.pure: $myId")

    val mappedId = myId.map(_ + 10)
    println(s"-- Monad[Identity].map (via extension function): $mappedId")

    val flatMappedId = myId.flatMap(x => (x + 10).pure[Identity])
    println(s"-- Monad[Identity].flatMap (via extension function): $flatMappedId")

    val unFlattenedId: Identity[Identity[Int]] = myId.map(_.pure[Identity])
    val flattenedId = unFlattenedId.flatten
    println(s"-- Monad[Identity].flatten (via extension function): $flattenedId")
  }

  {
    println

    import cats.instances.option._
    import cats.syntax.applicative._

    val myOpt = 32.pure[Option]
    println(s"-- Applicative.pure: $myOpt")
    val myNone = Option.empty[Int]
    println(s"-- Option.empty: $myNone")

    val mappedOpt = myOpt.map(_ + 10)
    println(s"-- Option.map: $mappedOpt")
    val mappedNone = myNone.map(_ + 10)
    println(s"-- Option.map $mappedNone")

    val flatMappedOpt = myOpt.flatMap(x => (x + 10).pure[Option])
    println(s"-- Option.flatMap: $flatMappedOpt")
    val flatMappedNone = myNone.flatMap(x => (x + 10).pure[Option])
    println(s"-- Option.flatMap $flatMappedNone")

    val unFlattenedOpt: Option[Option[Int]] = myOpt.map(_.pure[Option])
    val flattenedOpt = unFlattenedOpt.flatten
    println(s"-- Option.flatten: $flattenedOpt")
    val unFlattenedNone: Option[Option[Int]] = myNone.map(_.pure[Option])
    val flattenedNone = unFlattenedNone.flatten
    println(s"-- Option.flatten: $flattenedNone")
  }

  {
    println

    import cats.instances.list._
    import cats.syntax.applicative._

    val myList = List(32, 33, 34)
    println(s"-- List.apply: $myList")

    val mappedList = myList.map(_ + 10)
    println(s"-- List.map: $mappedList")

    val flatMappedList = myList.flatMap(x => (x + 10).pure[List])
    println(s"-- List.flatMap: $flatMappedList")

    val unFlattenedList: List[List[Int]] = myList.map(_.pure[List])
    val flattenedList = unFlattenedList.flatten
    println(s"-- List.flatten: $flattenedList")
  }

  println
}
