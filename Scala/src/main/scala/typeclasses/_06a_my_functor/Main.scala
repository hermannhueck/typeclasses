package typeclasses._06a_my_functor

import typeclasses._06a_my_functor.id.Identity
import typeclasses._06a_my_functor.lib.Functor

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

    import Functor.instances._

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

    import Functor.instances._

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

    import Functor.instances._
    import Functor.syntax._

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

    import Functor.instances._
    import Functor.syntax._

    val myList = List(32, 33, 34)
    println(s"-- List.apply $myList")

    val mappedList = myList.map(_ + 10)
    println(s"-- List.map: $mappedList")

    val fmappedList = myList.fmap(_ + 10)
    println(s"-- Functor[List].fmap (via extension function): $fmappedList")
  }

  println
}
