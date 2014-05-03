package yowfree


trait Free[F[_], A] {

  def flatMap(f: A => Free[B]): Free[B] = ???

  def map(f: A => B): Free[B] = ???

}

object Free {

  def liftF[F[_]: Functor, A](fa: => F[A]): NIHFree[F, A]


}


case class Suspend(functor: F) extends Free[A]


case class Return[A](a: A) extends Free[A]


