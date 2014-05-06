package kenbot.yowfree.tank.ai

import scala.collection.mutable
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.ArrayBuffer
import scalaz.Free._
import Moves._

class Exercise4Test extends FunSpec with ShouldMatchers {

  
  describe("loop") {
    it("should loop a script forever") {
      val asFarAsWeCareToGo = 50
      
      def interpret(ai: AI[Unit], counter: Int = 0): Int = ai.resume.fold({
        case Fire(next) => 
          if (counter < asFarAsWeCareToGo) interpret(next, counter + 1) 
          else counter
        case _ => -1
      }, _ => counter)
      
      val counter = interpret(loop(fire)) 
      counter should equal (asFarAsWeCareToGo)
    }
  }
  
  describe("script * n extension method") {
    it("should repeat the given number of times") {
      def interpret(ai: AI[Unit], counter: Int = 0): Int = ai.resume.fold({
        case Fire(next) => interpret(next, counter + 1) 
        case _ => counter
      }, _ => counter)
      
      val counter = interpret(fire * 3) 
      counter should equal (3)
    }
  }
  
  describe("when") {
    it("should return the given script when true") {
      val result = when(true)(fire)
      result should equal (fire)
    }
    it("should return Return(()) when true") {
      val result = when(false)(fire)
      result should equal (Return(()))
    }
  }
}
