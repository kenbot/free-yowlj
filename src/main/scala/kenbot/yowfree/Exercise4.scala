package kenbot.yowfree.tank.ai

import Moves._
import scalaz._
import Scalaz._
import scalaz.Free._

/**
 * This will be mixed in with the AI routines of the
 * tank game.  We will now build infinite looping 
 * scripts, and ones that diverge based on boolean conditions.
 * 
 * We are now using scalaz.Free instead of our own one, 
 * because looping using flatMap would otherwise blow the stack.  
 * 
 * scalaz.Free is built to avoid this, using a 3rd case "Gosub", which 
 * lifts the flatMap call onto the heap, like a Trampoline. It's more of an implementation 
 * detail though; conceptually it's still just Suspend and Return. It's a JVM thing; 
 * Haskell doesn't have this problem.
 */
trait TankAIMixin {
  
  /**
   * Exercise 4a.
   * 
   * Implement loop, such that the given AI script is repeated forever.
   */
  def loop(ai: AI[Unit]): AI[Unit] = ai >> loop(ai)
  
  /**
   * Exercise 4b. 
   * 
   * Implement when, which conditionally executes the given script.
   */
  def when(b: Boolean)(ai: => AI[Unit]): AI[Unit] = if (b) ai else Return(())
  
  final def unless(b: Boolean)(ai: => AI[Unit]): AI[Unit] = when(!b)(ai)
  
  
  /**
   * Exercise 4c. 
   * 
   * Implement an extension method * on AI[A], that will repeat
   * the script that many times. Usage: 
   * 
   * myScript * 5
   * 
   * "times" is assumed to be >= 1.
   * 
   */
  implicit class AIOps[A](ai: AI[A]) {
    def *(times: Int): AI[A] = {
      require(times >= 1)
      if (times == 1) ai else ai >> this * (times - 1)
    }
  }
  
  /**
   * Exercise 4d. Playtime!
   * 
   * Play with the tank AI scripts in StartingState.scala, run the game 
   * and watch the tanks move accordingly!  
   * 
   * Things you could try:
   *  - Compose new scripts in StartingState.scala for the tanks to run
   *  - Combine the primitive Move instructions in AI.scala into new composite commands.
   *  - Add a new instruction in the Move functor in AI.scala, and extend the 
   *                         DefaultTankAI interpreter in Interpreters.scala
   *  - Add a new Interpreter that modifies the behaviour of scripts in some interesting way.
   *  
   * 
   * 
   */
}