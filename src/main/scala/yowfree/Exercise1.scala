package yowfree

import scalaz._
import Scalaz._
import scalaz.{Free => _}

/** 
 * The Free monad generated by some functor F.
 */
trait Free[F[_], +A] {

  def flatMap[B](f: A => Free[F, B])
                (implicit F: Functor[F]): Free[F, B]

  def map[B](f: A => B)
            (implicit F: Functor[F]): Free[F, B]
}




case class Return[F[_], +A](a: A) extends Free[F, A] {
  
  /**
   * Exercise 1a.
   * Implement flatMap and map for Return. 
   */
  override def flatMap[B](f: A => Free[F, B])
                         (implicit F: Functor[F]): Free[F, B] = f(a)
                         
  override def map[B](f: A => B)
                     (implicit F: Functor[F]): Free[F, B] = Return(f(a))
}



case class Suspend[F[_], A](ffa: F[Free[F, A]]) extends Free[F, A] {
  /**
   * Exercise 1b.
   * Implement flatMap and map for Suspend. 
   * 
   * Hint: look at the type signatures! Just fit the pieces together.
   * 
   * You have only:
   *  - this: Free[F,A]
   *  - ffa: F[Free[F,A]]
   *  - ffa.map: F[Free[F,A]] => (Free[F,A] => B) => F[B]
   *  - this.flatMap: Free[F,A] => (A => Free[F, B]) => Free[F, B]
   *  - this.map: Free[F,A] => (A => B) => Free[F, B]
   *  - Suspend: F[Free[F,A]] => Free[F,A]
   *  - Return: A => Free[F,A]
   * 
   */
  override def flatMap[B](f: A => Free[F, B])
                         (implicit F: Functor[F]): Free[F, B] = Suspend(ffa map (_ flatMap f))
                         
  override def map[B](f: A => B)
                     (implicit F: Functor[F]): Free[F, B] = Suspend(ffa map (_ map f))
}

object Free {

  /**
   * Exercise 1c.
   * 
   * Implement liftF, lifting a functor into its free monad.
   */
  def liftF[F[_]: Functor, A](fa: => F[A]): Free[F, A] = ???

}





