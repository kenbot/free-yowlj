package kenbot.yowfree

import kenbot.yowfree.Free.liftF;
import scalaz.Functor
import scala.collection.mutable

// This example is based off the one in Runar Bjarnason's "Dead Simple Dependency Injection" talk.
// Highly recommended viewing.
//
// http://www.youtube.com/watch?v=ZasXwtTRkio


case class Key(id: String)

case class Value(value: String) {
    
  def +(amount: Int): Value = 
    Value((value.toInt + amount).toString)
}

/**
 * Exercise 3a. Instruction set
 * 
 * Implement ADT cases as subtypes of KVS, based on the following commands:
 * 
 * def put(key: Key, value: Value): Unit
 * def get(key: Key): Value
 * def delete(key: Key): Unit
 * 
 * Be careful with Get -- how is it different to Put and Delete?
 * How will the "Next" type parameter be used?
 * 
 */
case class Get[Next] // implement me
case class Put[Next] // implement me
case class Delete[Next] // implement me

// ADT translation of fantasy API
sealed trait KVS[+Next] {
  
  /**
   * Exercise 3b. Implement map, so KVS can be a functor.
   */
  def map[B](f: Next => B): KVS[B] = ???
}

object KVS {
  type Script[A] = Free[KVS, A]
  
  implicit val functor: Functor[KVS] = new Functor[KVS] {
    def map[A,B](kvs: KVS[A])(f: A => B): KVS[B] = kvs map f
  }
  
  
  /** 
   *  Exercise 3c. Lifting functions
   *  
   *  Implement functions that take regular input, but return
   *  KVS instances lifted into the Free monad.
   *  
   */
  def get(key: Key): Script[Value] = ???
  
  def put(key: Key, value: Value): Script[Unit] = ???
  
  def delete(key: Key): Script[Unit] = ???
  
  
  // Now we can write pure scripts using free monads!  Naturally, we'll exercise great
  // restraint with our newfound powers.
  
  
  val larceny: Script[Unit] = for {
    accountId <- get(Key("swiss-bank-account-id"))
    accountKey = Key(accountId.value)
    amount <- get(accountKey)                //  <-- 3d. Combine these 2 lines, using "modify".
    _ <- put(accountKey, amount + 1000000)   //  <-- 
    _ <- put(Key("bermuda-airport"), Value("getaway car"))
    _ <- delete(Key("tax-records"))
  } yield ()
  
  
  /**
   * Exercise 3d. Composing operations
   * 
   * It's a bit tiresome to write get-and-put every time we want to 
   * steal large sums of money and skip the country.
   * 
   * Write a composite function "modify", that Gets the value 
   * of the key, applies the modification and Puts it back to the store.
   * 
   * Change the "larceny" script, so it uses "modify" rather than get-and-put 
   * to modify the account balance.
   * 
   */
  def modify(key: Key, f: Value => Value): Script[Unit] = ???
  
  
  
  /**
   * Exercise 3e.  Pure interpreter
   * 
   * Write an interpreter, that recursively accumulates results into the immutable Map, returning the 
   * final result. 
   * 
   * Get, Put, and Delete should all do what you would expect. 
   *  
   * Hint: Pattern matching
   */
  def interpretPure[A](script: Script[A], dataStore: Map[Key, Value]): Map[Key, Value] = ???

  
  /**
   * Exercise 3f. Effectful interpreter
   * 
   * Write an interpreter, that reads each instruction in the script and 
   * mutates the given data store in place, returning unit.
   * 
   * Get, Put and Delete should all do what you what expect.
   */
  def interpretImpure[A](script: Script[A], dataStore: mutable.Map[Key, Value]): Unit = ???
}


