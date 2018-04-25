package typeclasses._06b_cats_functor

import cats.Functor
import typeclasses._06b_cats_functor.id.Identity

object Main extends App {

  println
  println("----- Using Functor[A].apply")

  {
    println

    import Identity.identityFunctor

    val myId = Identity(32)
    println(s"-- Identity.apply: $myId")

    val mappedId = Functor[Identity].map(myId)(_ + 10)
    println(s"-- Functor[Identity].map: $mappedId")

    val fmappedId = Functor[Identity].fmap(myId)(_ + 10)
    println(s"-- Functor[Identity].fmap: $fmappedId")
  }

  {
    println

    import cats.instances.option._

    val myOpt = Option(32)
    println(s"-- Option.apply: $myOpt")
    val myNone = Option.empty[Int]
    println(s"-- Option.empty[Int]: $myNone")

    val mappedOpt = Functor[Option].map(myOpt)(_ + 10)
    println(s"-- Functor[Option].map: $mappedOpt")
    val mappedNone = Functor[Option].map(myNone)(_ + 10)
    println(s"-- Functor[Option].map: $mappedNone")

    val fmappedOpt = Functor[Option].fmap(myOpt)(_ + 10)
    println(s"-- Functor[Option].fmap: $fmappedOpt")
    val fmappedNone = Functor[Option].fmap(myNone)(_ + 10)
    println(s"-- Functor[Option].fmap: $fmappedNone")
  }

  {
    println

    import cats.instances.list._

    val myList = List(32, 33, 34)
    println(s"-- List.apply $myList")

    val mappedList = Functor[List].map(myList)(_ + 10)
    println(s"-- Functor[List].map: $mappedList")

    val fmappedList = Functor[List].fmap(myList)(_ + 10)
    println(s"-- Functor[List].fmap: $fmappedList")
  }

  println
  println("----- Using Functor.syntax")

  {
    println

    import Identity.identityFunctor

    val myId = Identity(32)
    println(s"-- Identity.apply: $myId")

    val mappedId = myId.map(_ + 10)
    println(s"-- Functor[Identity].map (via extension function): $mappedId")

    val fmappedId = myId.fmap(_ + 10)
    println(s"-- Functor[Identity].fmap (via extension function): $fmappedId")
  }

  {
    println

    import cats.instances.option._
    import cats.syntax.functor._

    val myOpt = Option(32)
    println(s"-- Option.apply: $myOpt")
    val myNone = Option.empty[Int]
    println(s"-- Option.empty[Int]: $myNone")

    val mappedOpt = myOpt.map(_ + 10)
    println(s"-- Option.map: $mappedOpt")
    val mappedNone = myNone.map(_ + 10)
    println(s"-- Option.map: $mappedNone")

    val fmappedOpt = myOpt.fmap(_ + 10)
    println(s"-- Functor[Option].fmap (via extension function): $fmappedOpt")
    val fmappedNone = myNone.fmap(_ + 10)
    println(s"-- Functor[Option].fmap (via extension function): $fmappedNone")
  }

  {
    println

    import cats.instances.list._
    import cats.syntax.functor._

    val myList = List(32, 33, 34)
    println(s"-- List.apply $myList")

    val mappedList = myList.map(_ + 10)
    println(s"-- List.map: $mappedList")

    val fmappedList = myList.fmap(_ + 10)
    println(s"-- Functor[List].fmap (via extension function): $fmappedList")
  }

  println
  println("----- Using Functor.lift function")

  {
    println

    import cats.instances.option._
    import cats.instances.list._

    val idLift = Functor[Identity].lift[Int, Int](_ + 10)

    val myId = Identity(32)
    println(s"-- $myId")
    val liftedId = idLift(myId)
    println(s"-- $liftedId")

    val optLift = Functor[Option].lift[Int, Int](_ + 10)

    val myOpt = Option(32)
    println(s"-- $myOpt")
    val liftedOpt = optLift(myOpt)
    println(s"-- $liftedOpt")

    val listLift = Functor[List].lift[Int, Int](_ + 10)

    val myList = List(32, 33, 34)
    println(s"-- $myList")
    val liftedList = listLift(myList)
    println(s"-- $liftedList")
  }

  println
}
