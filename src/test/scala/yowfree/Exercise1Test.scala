package yowfree

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz._
import Scalaz._
import scalaz.{Free => _}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scalaz.Equal
import Arbitrary.arbitrary

object Instances {

  type FreeBox[A] = Free[Box, A]
  type FreeList[A] = Free[List, A]
  type FreeADT[A] = Free[ADT, A]
  

  implicit def freeEqual[F[_], A]: Equal[Free[F,A]] = Equal.equalA[Free[F,A]]
  implicit def boxEqual[A]: Equal[Box[A]] = Equal.equalA[Box[A]]
  implicit def adtEqual[A: Equal]: Equal[ADT[A]] = Equal.equalA[ADT[A]]
  
  def genSuspend[F[_], A](implicit arbF: Arbitrary[F[Free[F,A]]]): Gen[Free[F,A]] = 
    arbF.arbitrary map Suspend.apply
  
  def genReturn[F[_], A: Arbitrary]: Gen[Free[F,A]] = 
    Arbitrary.arbitrary[A] map Return.apply
    
  def genFree[F[_], A: Arbitrary](implicit arbF: Arbitrary[F[Free[F,A]]]) = 
    Gen.oneOf(genSuspend[F,A], genReturn[F, A])
  
  def arbitraryFree[F[_], A](implicit arbF: Arbitrary[F[Free[F,A]]], arbA: Arbitrary[A]): Arbitrary[Free[F,A]] = 
    Arbitrary(genFree[F,A])
    
  implicit def arbFreeList[A: Arbitrary]: Arbitrary[FreeList[A]] = arbitraryFree[List, A](
      implicitly[ Arbitrary[List[FreeList[A]]] ],   //arbitraryFunctor[List, Free[List, A]], 
      implicitly[ Arbitrary[A] ])
  
}

class Exercise1Test extends FunSpec with ShouldMatchers {

  import Instances._
  
  describe("Free.flatMap") {
    it("should obey monad laws") {
      monad.laws[FreeList].check
      //monad.laws[FreeBox].check
      //monad.laws[FreeADT].check
    }
  }
  
  
}

