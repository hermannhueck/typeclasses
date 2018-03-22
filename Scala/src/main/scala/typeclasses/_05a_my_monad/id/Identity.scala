package typeclasses._05a_my_monad.id

import typeclasses._05a_my_monad.lib.Monad

final case class Identity[T](value: T)

object Identity {

  implicit val identityMonad: Monad[Identity] = new Monad[Identity] {
    override def pure[A](a: A): Identity[A] = Identity(a)
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa.value)
  }

  implicit class IdentityMonad[A](id: Identity[A]) {
    def flatMap[B](f: A => Identity[B])(implicit monad: Monad[Identity]): Identity[B] = monad.flatMap(id)(f)
    def flatten(implicit monad: Monad[Identity]): Identity[A] = monad.flatten(id.asInstanceOf[Identity[Identity[A]]])
    def map[B](f: A => B)(implicit monad: Monad[Identity]): Identity[B] = monad.map(id)(f)
    def fmap[B](f: A => B)(implicit monad: Monad[Identity]): Identity[B] = monad.fmap(id)(f)
  }
}
