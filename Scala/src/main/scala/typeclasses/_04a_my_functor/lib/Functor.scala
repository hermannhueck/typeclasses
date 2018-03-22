package typeclasses._04a_my_functor.lib

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)
}

object Functor {

  def apply[F[_]](implicit instance : Functor[F]) : Functor[F] = instance
  
  object instances {

    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

    implicit val listFunctor: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }
  }

  object syntax {

    implicit class OptionFunctor[A](opt: Option[A]) {
      def fmap[B](f: A => B)(implicit functor: Functor[Option]): Option[B] = functor.map(opt)(f)
    }

    implicit class ListFunctor[A](list: List[A]) {
      def fmap[B](f: A => B)(implicit functor: Functor[List]): List[B] = functor.map(list)(f)
    }
  }
}
