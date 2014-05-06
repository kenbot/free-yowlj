package kenbot.yowfree

import scala.collection.mutable

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import KVS._

class Exercise3Test extends FunSpec with ShouldMatchers {

  def testInterpreter(script: Script[Unit], dataStore: Map[Key, Value]): Map[Key, Value] = script match {
    case Suspend(Get(key, nextF)) => interpretPure(nextF(dataStore(key)), dataStore)
    case Suspend(Put(key, value, next)) => interpretPure(next, dataStore + (key -> value))
    case Suspend(Delete(key, next)) => interpretPure(next, dataStore - key)
    case Return(_) => dataStore 
  }
  
  describe("Pure interpreter") {
    it("should interpret Put correctly") {
      val start = Map[Key, Value]()
      val end = interpretPure(put(Key("a"), Value("x")), start)
      end should equal (Map(Key("a") -> Value("x")))
    }
    
    it("should interpret Delete correctly") {
      val start = Map(Key("DELETEME") -> Value("XXX"))
      val end = interpretPure(delete(Key("DELETEME")), start)
      end should equal (Map())
    }
    
    
    it("should follow a script faithfully") {
      val script = for {
        a <- get(Key("a"))
        _ <- put(Key("b"), a)
        _ <- delete(Key("c"))
      } yield ()
      
      val start = Map(Key("a") -> Value("apples"), 
                      Key("c") -> Value("DELETE ME"))
      val end = interpretPure(script, start)
      
      end should equal (Map(Key("a") -> Value("apples"),
                            Key("b") -> Value("apples")))
    }
  }

  describe("Impure interpreter") {
    it("should interpret Put correctly") {
      val mutableStore = mutable.Map[Key, Value]()
      interpretImpure(put(Key("a"), Value("x")), mutableStore)
      mutableStore should equal (Map(Key("a") -> Value("x")))
    }
    
    it("should interpret Delete correctly") {
      val start = Map(Key("DELETEME") -> Value("XXX"))
      val end = interpretPure(delete(Key("DELETEME")), start)
      end should equal (Map())
    }
    
    
    it("should follow a script faithfully") {

      val script = for {
        a <- get(Key("a"))
        _ <- put(Key("b"), a)
        _ <- delete(Key("c"))
      } yield ()
      
      val mutableStore = mutable.Map(Key("a") -> Value("apples"), 
                                     Key("c") -> Value("DELETE ME"))
                                     
      interpretImpure(script, mutableStore)
      
      mutableStore should equal (Map(Key("a") -> Value("apples"),
                                     Key("b") -> Value("apples")))
    }
  }
}
