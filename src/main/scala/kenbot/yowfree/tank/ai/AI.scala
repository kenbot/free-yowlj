package kenbot.yowfree.tank.ai

import Moves._
import scalaz._
import Scalaz._
import scalaz.Free._
import kenbot.yowfree.tank.model.Tank
import kenbot.yowfree.tank.maths.Vec
import kenbot.yowfree.tank.maths.Angle
import kenbot.yowfree.tank.model.Entity
import kenbot.yowfree.tank.model.Physical

// ADT for tank moves
sealed trait Move[+A] {
  def map[B](f: A => B): Move[B] = this match {
    case Accelerate(next) => Accelerate(f(next))
    case RotateLeft(upTo, next) => RotateLeft(upTo, f(next))
    case RotateRight(upTo, next) => RotateRight(upTo, f(next))
    case Delay(next) => Delay(f(next))
    case Fire(next) => Fire(f(next))
    case FindNearestTank(onFoundTank) => FindNearestTank(onFoundTank andThen f) 
    case AngleTo(toPos, onAngle) => AngleTo(toPos, onAngle andThen f)
    case IsAt(pos, onAt) => IsAt(pos, onAt andThen f)
    case IsFacing(angle, onFacing) => IsFacing(angle, onFacing andThen f)
    case Me(onMe) => Me(onMe andThen f)
  }
}
case class Accelerate[A](next: A) extends Move[A] 
case class RotateLeft[A](upTo: Option[Angle], next: A) extends Move[A]
case class RotateRight[A](upTo: Option[Angle], next: A) extends Move[A]
case class Delay[A](next: A) extends Move[A]
case class Fire[A](next: A) extends Move[A]
case class FindNearestTank[A](onFoundTank: Tank => A) extends Move[A]
case class AngleTo[A](toPos: Vec, onAngle: Angle => A) extends Move[A]
case class IsAt[A](pos: Vec, onAt: Boolean => A) extends Move[A]
case class IsFacing[A](angle: Angle, onFacing: Boolean => A) extends Move[A]
case class Me[A](onMe: Entity => A) extends Move[A]
  

// Functor definition
object Move {
  implicit def functor: Functor[Move] =  new Functor[Move] {
    def map[A,B](move: Move[A])(f: A => B): Move[B] = move map f
  }
}

object Moves extends AdvancedMoves

// Lifting functions.  Note that liftF is implicit within the trait.
trait BasicMoves {
  type AI[A] = Free[Move, A]
  
  val AIDone: AI[Unit] = Return(())
  
  def accelerate: AI[Unit] = liftF(Accelerate(()))

  def rotateLeft: AI[Unit] = liftF(RotateLeft(None, ()))
  
  def rotateRight: AI[Unit] = liftF(RotateRight(None, ()))
  
  def rotateLeftUpTo(upTo: Angle): AI[Unit] = liftF(RotateLeft(Some(upTo), ()))
  
  def rotateRightUpTo(upTo: Angle): AI[Unit] = liftF(RotateRight(Some(upTo), ()))
  
  def delay: AI[Unit] = liftF(Delay(()))
  
  def fire: AI[Unit] = liftF(Fire(()))
  
  def findNearestTank: AI[Tank] = liftF(FindNearestTank(identity))
  
  def angleTo(pos: Vec): AI[Angle] = liftF(AngleTo(pos, identity))
  
  def isAt(pos: Vec): AI[Boolean] = liftF(IsAt(pos, identity))
  
  def isFacing(angle: Angle): AI[Boolean] = liftF(IsFacing(angle, identity))
  
  def me: AI[Entity] = liftF(Me(identity))
  
}

// Fancy composite tank moves
trait AdvancedMoves extends BasicMoves with TankAIMixin {

  def aimAtTank(tank: Tank): AI[Unit] = for {
    angle <- angleTo(tank.pos)
    _ <- rotateTowards(angle)
  } yield ()
  
  def aimAwayFrom(tank: Tank): AI[Unit] = for {
    angle <- angleTo(tank.pos)
    _ <- rotateTowards(angle.opposite)
  } yield ()
  
  def fireAtTank(tank: Tank): AI[Unit] = aimAtTank(tank) >> fire
  
  def rotateTowards(angle: Angle): AI[Unit] = for {
    ok <- isFacing(angle)
    tank <- me
    _ <- unless(ok) {
           val left = tank.facing isLeftOf angle
           val rotationAction = if (left) rotateLeftUpTo(angle)
                                else rotateRightUpTo(angle)
           rotationAction >> rotateTowards(angle)
         }
  } yield ()
  
  def distanceTo(e: Entity): AI[Double] = me.map(_ distanceTo e)
  
  def moveTo(pos: Vec): AI[Unit] = for {
    arrived <- isAt(pos)
    _ <- unless(arrived)(for {
      angle <- angleTo(pos)
      _ <- rotateTowards(angle)
      _ <- accelerate
      _ <- moveTo(pos)
    } yield ())
  } yield ()
  
  def searchAndDestroy: AI[Unit] = for {
    tank <- findNearestTank
    angle <- angleTo(tank.pos)
    _ <- fire
  } yield ()
  
  
  def whenDistance(ph: Physical, p: Double => Boolean)(then: AI[Unit]): AI[Unit] = for {
    me <- me
    _ <- when(p(me distanceTo ph))(then)
  } yield ()
  
  def patrol(squareSize: Double): AI[Unit] = for {
    me <- me
    Vec(x,y) = me.pos
    _ <- moveTo(Vec(x, y + squareSize))
    _ <- fire
    _ <- moveTo(Vec(x + squareSize, y + squareSize))
    _ <- fire
    _ <- moveTo(Vec(x + squareSize, y))
    _ <- fire
    _ <- moveTo(Vec(x, y))
  } yield ()
}

  

