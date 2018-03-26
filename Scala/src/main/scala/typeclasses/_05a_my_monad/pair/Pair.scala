package typeclasses._05a_my_monad.pair

// import typeclasses._05a_my_monad.lib.{Monad, Monoid}

final case class Pair[A, B](v1: A, v2: B)

/*

!!!
It's too complicated to provide a Pair Monad.
I stopped this experiment for the time being
... and possibly resume it later.
!!!

object Pair {

  implicit def pairMonoid[A, B](implicit ma: Monoid[A], mb: Monoid[B]): Monoid[Pair[A, B]] = new Monoid[Pair[A, B]] {
    override def empty: Pair[A, B] = Pair(ma.empty, mb.empty)
    override def combine(p1: Pair[A, B], p2: Pair[A, B]): Pair[A, B] = Pair(ma.combine(p1.v1, p2.v1), mb.combine(p1.v2, p2.v2))
  }

  implicit def pairMonad[X](implicit ma: Monoid[X]): Monad[Pair[X, _]] = {

    type MyPair[Y] = Pair[X, Y]

    new Monad[MyPair] {

      override def pure[A](a: A): Pair[X, A] = Pair(ma.empty, a)

      override def flatMap[A, B](fa: MyPair[A])(f: A => MyPair[B]): MyPair[B] = {
        val Pair(x, y) = f(fa.v2)
        Pair(ma.combine(fa.v1, x), y)
      }
    }.asInstanceOf[Monad[Pair[X, _]]]
  }

  implicit class PairMonad[X, A](pair: Pair[X, A]) {

    type MyPair[A] = Pair[X, A]

    def map[B](f: A => B)(implicit monad: Monad[MyPair]): MyPair[B] = monad.map(pair)(f)
    //def map[B](f: A => B)(implicit monad: Monad[MyPair]): MyPair[B] = monad.map(pair)(f)
    def flatMap[B](f: A => MyPair[B])(implicit monad: Monad[MyPair]): MyPair[B] = monad.flatMap(pair)(f)
    def flatten(implicit monad: Monad[MyPair]): MyPair[A] = monad.flatten(pair.asInstanceOf[MyPair[MyPair[A]]])
  }
}
*/
