package typeclasses.mycats

import typeclasses.mycats.lib.Functor

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

    import typeclasses.mycats.Identity.identityFunctor

    val mappedId = Functor[Identity].map(myId)(_ + 10)

    println(s"-- $mappedId")
  }

  {
    println

    val myId = Identity(32)
    println(s"-- $myId")

    import typeclasses.mycats.Identity.identityFunctor

    val mappedId = myId.map(_ + 10)
    val fmappedId = myId.fmap(_ + 10)

    println(s"-- $mappedId")
    println(s"-- $fmappedId")
  }

  {
    println

    import typeclasses.mycats.lib.Functor.option._

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

    val myList = List(32, 33, 34)
    println(s"-- $myList")

    import typeclasses.mycats.lib.Functor.list._

    val mappedList = myList.map(_ + 10)
    val fmappedList = myList.fmap(_ + 10)

    println(s"-- $mappedList")
    println(s"-- $fmappedList")
  }

  {
    println

    val idLift = Functor[Identity].lift[Int, Int](_ + 10)

    val myId = Identity(32)
    println(s"-- $myId")
    val liftedId = idLift(myId)
    println(s"-- $liftedId")

    import typeclasses.mycats.lib.Functor.option._

    val optLift = Functor[Option].lift[Int, Int](_ + 10)

    val myOpt = Option(32)
    println(s"-- $myOpt")
    val liftedOpt = optLift(myOpt)
    println(s"-- $liftedOpt")

    import typeclasses.mycats.lib.Functor.list._

    val listLift = Functor[List].lift[Int, Int](_ + 10)

    val myList = List(32, 33, 34)
    println(s"-- $myList")
    val liftedList = listLift(myList)
    println(s"-- $liftedList")
  }

  println
}
