package typeclasses._06a_my_functor.id

import typeclasses._06a_my_functor.lib.Functor

final case class Identity[A](value: A)

object Identity {

  implicit val identityFunctor: Functor[Identity] = new Functor[Identity] {
    override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.value))
  }

  implicit class IdentityFunctor[A](id: Identity[A]) {
    def map[B](f: A => B)(implicit functor: Functor[Identity]): Identity[B] = functor.map(id)(f)
    def fmap[B](f: A => B)(implicit functor: Functor[Identity]): Identity[B] = functor.map(id)(f)
  }
}
