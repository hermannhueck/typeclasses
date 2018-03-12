package typeclasses.myfunctor.lib

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
}

object Functor {

  def apply[F[_]](implicit instance : Functor[F]) : Functor[F] = instance
  
  object option {

    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

    implicit class OptionFunctor[T](opt: Option[T]) {
      def map[U](f: T => U)(implicit functor: Functor[Option]): Option[U] = functor.map(opt)(f)
      def fmap[U](f: T => U)(implicit functor: Functor[Option]): Option[U] = functor.fmap(opt)(f)
    }
  }

  object list {

    implicit val listFunctor: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit class ListFunctor[T](opt: List[T]) {
      def map[U](f: T => U)(implicit functor: Functor[List]): List[U] = functor.map(opt)(f)
      def fmap[U](f: T => U)(implicit functor: Functor[List]): List[U] = functor.fmap(opt)(f)
    }
  }
}
