package typeclasses._07b_cats_monad.id

import cats.Monad

final case class Identity[T](value: T)

object Identity {

  implicit val identityMonad: Monad[Identity] = new Monad[Identity] {
    override def pure[A](a: A): Identity[A] = Identity(a)
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa.value)
    // tailRecM implementation not used
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = ???
  }

  implicit class IdentityMonad[A](id: Identity[A]) {
    def flatMap[B](f: A => Identity[B])(implicit monad: Monad[Identity]): Identity[B] = monad.flatMap(id)(f)
    def flatten(implicit monad: Monad[Identity]): Identity[A] = monad.flatten(id.asInstanceOf[Identity[Identity[A]]])
    def map[B](f: A => B)(implicit monad: Monad[Identity]): Identity[B] = monad.map(id)(f)
  }
}
