package imagesprinkler

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.TIMEOUT
import scala.collection.mutable.{ Map => MMap, Buffer => MBuffer }

import imagesprinkler.sprinkler._


object Backend {
  val ShutdownTimeout = 10000

  case class RegisterSprinkler(spinkler:Sprinkler)
  case class UnregisterSprinkler(spinkler:Sprinkler)

  trait Listener extends Actor
  case class RegisterListener(listener:Listener)
  case class UnregisterListener(listener:Listener)
}

class Backend extends Actor {

  val sprinklers = MBuffer[Sprinkler]()
  val listeners = MBuffer[Backend.Listener]()

  def act() {
    var running = true

    while (running || sprinklers.size > 0 || listeners.size > 0) {

      receiveWithin(Backend.ShutdownTimeout) {

        case Backend.RegisterSprinkler(sprinkler) =>
          sprinklers += sprinkler

        case Backend.UnregisterSprinkler(sprinkler) =>
          sprinklers -= sprinkler

        case Backend.RegisterListener(listener) =>
          listeners += listener

        case Backend.UnregisterListener(listener) =>
          listeners -= listener

        case Send(photo) => 
          println("Sending photo to " + sprinklers.size + " sprinklers")
          sprinklers.foreach { s => s ! Send(photo) }

        case Shutdown => 
          println("Shuting down " + sprinklers.size + " sprinklers and " + listeners.size + " listeners")
          sprinklers.foreach { s => s ! Shutdown }
          listeners.foreach  { l => l ! Shutdown }
          running = false

        case TIMEOUT if !running =>
          println("Timed out waiting for Shutdown of sprinklers or listeners, exiting")
          sprinklers.clear
          listeners.clear

        case message @ Started(sprinkler, instance @ PhotoInstance(photo, key)) => 
          // Forward to listeners
          listeners.foreach { l => l ! message }

        case message @ InProgress(sprinkler, instance @ PhotoInstance(photo, key), info) => 
          // Forward to listeners
          listeners.foreach { l => l ! message }

        case message @ Complete(sprinkler, instance @ PhotoInstance(photo, key)) => 
          // Forward to listeners
          listeners.foreach { l => l ! message }

        case message @ Error(sprinkler, instance @ PhotoInstance(photo, key), info) => 
          // Forward to listeners
          listeners.foreach { l => l ! message }
      }
    }
  }
  

}