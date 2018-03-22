package typeclasses._05a_my_monad.lib

import scala.language.higherKinds

trait Monad[F[_]] {

  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f) // alias for map

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
}
