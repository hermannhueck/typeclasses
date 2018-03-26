package typeclasses._05a_my_monad.lib

import scala.language.higherKinds

trait Monoid[A] {
  def empty: A
  def combine(a1: A, a2: A): A
  def |+|(a1: A, a2: A): A = combine(a1, a2)
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A] // pure may be named as: return, unit, point
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Monad[F[_]] extends Applicative[F] {

  // the intrinsic functions of a Monad are flatMap and pure.

  def pure[A](a: A): F[A] // pure may be named as: return, unit, point
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // all functions below are some how implemented in terms of flatMap and pure.

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f) // alias for map

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(f => map(fa)(a => f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
}

object Monad {

  def apply[F[_]](implicit instance : Monad[F]) : Monad[F] = instance

  implicit class pure[A](val a: A) extends AnyVal {
    def pure[F[_]](implicit F: Monad[F]): F[A] = F.pure(a)
  }

  object instances {

    implicit val optionMonad: Monad[Option] = new Monad[Option] {
      override def pure[A](a: A): Option[A] = Option(a)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    implicit val listMonad: Monad[List] = new Monad[List] {
      override def pure[A](a: A): List[A] = List(a)
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }
  }

  object syntax {

    implicit class OptionMonad[A](id: Option[A]) {
      def ap[B](ff: Option[A => B])(implicit monad: Monad[Option]): Option[B] = monad.ap(ff)(id)
    }

    implicit class ListMonad[A](id: List[A]) {
      def ap[B](ff: List[A => B])(implicit monad: Monad[List]): List[B] = monad.ap(ff)(id)
    }
  }
}
