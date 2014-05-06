package kenbot.yowfree.tank.ai

import Moves._
import scalaz._
import Scalaz._
import scalaz.Free._
import kenbot.yowfree.tank.model.Tank
import kenbot.yowfree.tank.model.World
import kenbot.yowfree.tank.model.Entity
import kenbot.yowfree.tank.maths.Angle
import scala.util.Random
import kenbot.yowfree.tank.model.EntityId

trait MoveInterpreter extends ((World, Entity) => World) {
  
  type WorldChange = (World, EntityId) => World
  
  
  final def apply(world: World, entity: Entity): World = {
    // Resume is a method on Free in Scalaz. It unwraps a Free, returning 
    // A \/ F[Free[F,A]].  We are using it to consume one instruction in the 
    // script, saving the rest for later.
    entity.ai.resume.fold(
        interpretMove(_)(world, entity.id), 
        _ => world)
  }
  
  protected def interpretMove(move: Move[AI[Unit]]): WorldChange
  
  // Convenience methods to reduce repetition in the Interpreter instances.
  
  protected final def doNothing(nextAI: AI[Unit]): WorldChange = {
    (world, id) => world.updateEntity(id)(_ withAI nextAI)
  }
  
  protected final def updateEntity(nextAI: AI[Unit])(f: Entity => Entity): WorldChange = {
    (world, id) => world.updateEntity(id)(e => f(e) withAI nextAI)
  }
  
  protected final def updateWorld(nextAI: AI[Unit])(f: World => World): WorldChange = {
    (world, id) => f(world).updateEntity(id)(_ withAI nextAI)
  }
  
  protected final def updateEntityInWorld(nextAI: AI[Unit])(f: (World, Entity) => World): WorldChange = {
    (world, id) => world.find(id).fold(world) { e => 
      f(world, e).withEntity(e withAI nextAI)
    }
  }
  
  protected final def observeEntity[A](nextAI: A => AI[Unit])(f: Entity => A): WorldChange = {
    (world, id) => world.updateEntity(id)(e => e withAI nextAI(f(e)))
  }
  
  protected final def observeWorld[A](nextAI: A => AI[Unit])(f: World => A): WorldChange = {
    (world, id) => world.updateEntity(id)(e => e withAI nextAI(f(world)))
  }
  
  protected final def observeEntityInWorld[A](nextAI: (A => AI[Unit]))(f: (World, Entity) => A): WorldChange = {
    (world, id) => world.updateEntity(id)(e => e withAI nextAI(f(world, e)))
  }
}

