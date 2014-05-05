
package yowfree {
  
  case class Box[A](a: A) {
    def map[B](f: A => B): Box[B] = Box(f(a))
  }
  
  sealed trait ADT[+A] {
    def map[B](f: A => B): ADT[B] = this match {
      case Bell(next) => Bell(f(next))
      case Output(str, next) => Output(str, f(next))
    }
  }
  case class Bell[+A](next: A) extends ADT[A]
  case class Output[+A](str: String, next: A) extends ADT[A]
  
}


package object yowfree {
  import scalaz.{Functor, Monad}
  
  type ??? = Nothing
  
  implicit val function0Functor: Functor[Function0] = new Functor[Function0] {
    override def map[A,B](f0: Function0[A])(f: A => B): Function0[B] = { () => f(f0()) }
  }
  
  implicit val adtFunctor: Functor[ADT] = new Functor[ADT] {
    override def map[A,B](adt: ADT[A])(f: A => B): ADT[B] = adt map f
  }
  
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A,B](box: Box[A])(f: A => B): Box[B] = box map f
  }
  
  implicit def freeMonad[F[_]: Functor]: Monad[({type f[x] = Free[F, x]})#f] = {
    new Monad[({type f[x] = Free[F, x]})#f] {
      def point[A](a: => A) = Return(a)
      def bind[A, B](a: Free[F, A])(f: A => Free[F, B]) = a flatMap f
    }
  }
}