package typeclasses._00a_implicit_conversion

object Main extends App {

  println

  {
    import scala.language.implicitConversions

    implicit def string2int(s: String): Int = Integer.parseInt(s)

    val x: Int = "5" // !! Assign a string to an int val

    def useInt(x: Int): Unit = println("Int value: " + x)

    useInt("42") // !! Pass a string to an int param
  }

  println

  {
    import scala.language.implicitConversions

    case class Foo(value: String) {
      override def toString: String = s"Foo($value)"
    }
    case class Bar(value: String) {
      override def toString: String = s"Bar($value)"
    }

    implicit def foo2Bar(foo: Foo): Bar = Bar(foo.value)

    val bar: Bar = Foo("foo")

    println(bar)
  }

  println

  {
    import scala.language.implicitConversions

    object foobar {
      case class Foo(value: String) {
        override def toString: String = s"Foo($value)"
      }
      case class Bar(value: String) {
        override def toString: String = s"Bar($value)"
      }

      object Foo {
        implicit def foo2Bar(foo: Foo): Bar = Bar(foo.value)
      }
    }

    import foobar.{Foo, Bar}

    val bar: Bar = Foo("foo")

    println(bar)
  }

  println

  {
    import scala.language.implicitConversions

    object foobar {
      case class Foo(value: String) {
        override def toString: String = s"Foo($value)"
      }
      case class Bar(value: String) {
        override def toString: String = s"Bar($value)"
      }

      object Bar {
        implicit def foo2Bar(foo: Foo): Bar = Bar(foo.value)
      }
    }

    import foobar.{Foo, Bar}

    val bar: Bar = Foo("foo")

    println(bar)
  }

  println

  {
    import scala.language.implicitConversions

    object foobar {
      case class Foo[A](value: A) {
        override def toString: String = s"Foo(${value.toString})"
      }
      case class Bar[A](value: A) {
        override def toString: String = s"Bar(${value.toString})"
      }

      object Foo {
        implicit def foo2Bar[A](foo: Foo[A]): Bar[A] = Bar(foo.value)
      }
    }

    import foobar.{Foo, Bar}

    val barString: Bar[String] = Foo("foo")
    val barInt: Bar[Int] = Foo(42)

    println(barString)
    println(barInt)
  }

  println

  {
    import scala.language.implicitConversions

    object foobar {
      case class Foo[A](value: A) {
        override def toString: String = s"Foo(${value.toString})"
      }
      case class Bar[A](value: A) {
        override def toString: String = s"Bar(${value.toString})"
      }

      object Bar {
        implicit def foo2Bar[A](foo: Foo[A]): Bar[A] = Bar(foo.value)
      }
    }

    import foobar.{Foo, Bar}

    val barString: Bar[String] = Foo("foo")
    val barInt: Bar[Int] = Foo(42)

    println(barString)
    println(barInt)
  }

  println

  {
    import scala.language.implicitConversions

    object foobar {
      case class Bar[A](value: A) {
        override def toString: String = s"Bar(${value.toString})"
      }

      object Bar {
        implicit def foo2Bar[A](a: A): Bar[A] = Bar(a)
      }
    }

    import foobar.Bar

    val barString: Bar[String] = "foo"
    val barInt: Bar[Int] = 42

    println(barString)
    println(barInt)
  }

  println
}
