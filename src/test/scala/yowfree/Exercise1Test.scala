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

class Exercise1Test extends FunSpec with ShouldMatchers {

  describe("Return") {
    val ret1 = Return[List, Int](1)
    
    describe("flatMap") {
      it("should behave as expected given a Return") {
        ret1.flatMap(a => Return(a + 1)) should equal (Return(2))
      }
      
      it("should behave as expected given a Suspend") { 
        ret1.flatMap(a => 
          Suspend[List, Int](List(
              Return(a + 1), 
              Return(a + 2)))) should equal (
                  Suspend[List, Int](List(
                      Return(2), 
                      Return(3))))
      }
    }
    
    describe("map") {
      it("should behave as expected") {
        ret1.map(_ + 1) should equal (Return(2))
      }
    }
  }
  
  describe("Suspend") {
    val suspendList = Suspend[List, Int](List(
        Return(1), 
        Return(2), 
        Return(3)))
        
    describe("flatMap") {
      it("should behave as expected given a Return") {
        
        suspendList.flatMap(a => Return(a + 1)) should equal (
            Suspend[List, Int](List(
                Return(2), 
                Return(3), 
                Return(4))))
      }
      
      it("should behave as expected given a Suspend") {
  
        suspendList.flatMap(a => 
          Suspend[List, Int](List(
              Return(a + 1), 
              Return(a + 2)))) should equal (
                  Suspend[List, Int](List(
                      Suspend[List, Int](List(Return(2), Return(3))), 
                      Suspend[List, Int](List(Return(3), Return(4))), 
                      Suspend[List, Int](List(Return(4), Return(5))))))
      }
    }
  
    describe("map") {
      it("should behave as expected") {
        
        suspendList.map(_ + 1) should equal (
            Suspend[List, Int](List(
                Return(2), 
                Return(3), 
                Return(4))))
      }
    }
  }

  describe("liftF") {
    it("should behave as expected") {
      Free.liftF[List, Int](List(1,2,3)) should equal (
          Suspend[List, Int](List(
              Return(1), 
              Return(2), 
              Return(3))))
    }
  }
  
}

