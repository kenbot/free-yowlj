package yowfree

import Trampolines._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec

object EvenOdd {
  def even[A](list: List[A]): Trampoline[Boolean] = list match {
    case Nil => Return(true)
    case _ :: rest => Suspend(() => odd(rest))
  }
  
  def odd[A](list: List[A]): Trampoline[Boolean] = list match {
    case Nil => Return(false)
    case _ :: rest => Suspend(() => even(rest))
  }
}


class Exercise2Test extends FunSpec with ShouldMatchers {

  import Trampolines._
  
  describe("The trampoline interpreter") {
    it("should run a computation all the way through") {
      val even = runTrampoline(EvenOdd.even((1 to 9).toList))
      even should be (false)
      
      val even2 = runTrampoline(EvenOdd.even((1 to 10).toList))
      even2 should be (true)
    }
    
    it("shouldn't blow the stack") {
      runTrampoline(EvenOdd.even((1 to 10000).toList))
    }
  }
  
  describe("The trampolined solution") {
    import ListAppendRighteousTrampoline.runSolution
    it("shouldn't blow the stack") {
      runSolution((1 to 9999).toList, (10000 to 20000).toList)
    }
    
    it("should append empty list1 correctly") {
      runSolution(Nil, List(1,2,3)) should equal (List(1,2,3))
    }
    
    it("should append empty list2 correctly") {
      runSolution(List(1,2,3), Nil) should equal (List(1,2,3))
    }
    
    it("should append two empty lists correctly") {
      runSolution(Nil, Nil) should equal (Nil)
    }
    
    it("should append two non-empty lists correctly") {
      runSolution(List(1,2,3), List(4,5,6)) should equal (List(1,2,3,4,5,6))
    }
  }
}

