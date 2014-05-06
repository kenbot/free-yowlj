package kenbot.yowfree.tank.app
import kenbot.yowfree.tank.model.Tank
import kenbot.yowfree.tank.model.World
import kenbot.yowfree.tank.model.TankGame
import kenbot.yowfree.tank.ai.EasyTankAI
import kenbot.yowfree.tank.maths.Vec
import kenbot.yowfree.tank.maths.Dim
import kenbot.yowfree.tank.ai.Moves
import kenbot.yowfree.tank.ai.HardTankAI
import kenbot.yowfree.tank.ai.TruceTankAI
import scalaz._ 
import Scalaz._

object StartingState {
  import Moves._
  
  
  val tanks = List(
    Tank("1", Vec(10,10)) withAI loop(for {
      t <- findNearestTank
      _ <- aimAtTank(t)
      _ <- whenDistance(t, _ > 50)(accelerate * 4)
      _ <- fire
      _ <- delay 
    } yield ()),
    
    Tank("2", Vec(500, 500)) withAI loop(for {
      t <- findNearestTank
      _ <- whenDistance(t, _ < 100) (for {
             _ <- aimAwayFrom(t)
             _ <- accelerate * 20
           } yield ())
      _ <- patrol(10)
    } yield ()),
    
    
    Tank("3", Vec(200,400)) withAI loop(patrol(100) >> fire),
    
    Tank("4", Vec(300,100)) withAI loop(for {
      _ <- rotateLeft * 2
      _ <- fire
      _ <- accelerate * 4
    } yield ()))
      
  val world = World(Dim(1000,700), tanks)
  val game = TankGame(world, TruceTankAI, 0)
}

