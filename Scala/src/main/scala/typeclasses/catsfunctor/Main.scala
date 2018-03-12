package typeclasses.catsfunctor

import cats.Functor

object Main extends App {

  {
    println

    val myId = Identity(32)
    println(s"-- $myId")

    val functor = new Functor[Identity] {
      override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.value))
    }

    val mappedId = functor.map(myId)(_ + 10)

    println(s"-- $mappedId")
  }

  {
    println

    val myId = Identity(32)
    println(s"-- $myId")

    import Identity.identityFunctor

    val mappedId = Functor[Identity].map(myId)(_ + 10)

    println(s"-- $mappedId")
  }

  {
    println

    val myId = Identity(32)
    println(s"-- $myId")

    import Identity.identityFunctor

    val mappedId = myId.map(_ + 10)
    val fmappedId = myId.fmap(_ + 10)

    println(s"-- $mappedId")
    println(s"-- $fmappedId")
  }

  {
    println

    import cats.instances.option._
    import cats.syntax.functor._

    val myOpt = Option(32)
    println(s"-- $myOpt")

    val mappedOpt = myOpt.map(_ + 10)
    val fmappedOpt = myOpt.fmap(_ + 10)
    println(s"-- $mappedOpt")
    println(s"-- $fmappedOpt")

    val myNone = Option.empty[Int]
    println(s"-- $myNone")

    val mappedNone = myNone.map(_ + 10)
    val fmappedNone = myNone.fmap(_ + 10)
    println(s"-- $mappedNone")
    println(s"-- $fmappedNone")
  }

  {
    println

    import cats.instances.list._
    import cats.syntax.functor._

    val myList = List(32, 33, 34)
    println(s"-- $myList")

    val mappedList = myList.map(_ + 10)
    val fmappedList = myList.fmap(_ + 10)

    println(s"-- $mappedList")
    println(s"-- $fmappedList")
  }

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
