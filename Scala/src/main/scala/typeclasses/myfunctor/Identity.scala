package typeclasses.myfunctor

import typeclasses.myfunctor.lib.Functor

final case class Identity[T](value: T)

object Identity {

  implicit val identityFunctor: Functor[Identity] = new Functor[Identity] {
    override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.value))
  }

  implicit class IdentityFunctor[T](id: Identity[T]) {
    def map[U](f: T => U)(implicit functor: Functor[Identity]): Identity[U] = functor.map(id)(f)
    def fmap[U](f: T => U)(implicit functor: Functor[Identity]): Identity[U] = functor.fmap(id)(f)
  }
}