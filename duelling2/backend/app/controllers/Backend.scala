package se.netlight.fn.imagesprinkler

import scala.actors.Actor
import scala.actors.Actor._

object Main {
  
  def main(args:Array[String]) {
    val backend = new Backend()

    new DebugSprinkler(backend).start
    new ExternalJsonSprinkler(backend, "url").start

    println("sending photo")
    backend.send(Photo("title", "desc", null))
    println("sending shutdown")
    backend.shutdown
    println("completed")
  }

}

class Backend {

  var sprinklers = List[Actor]()

  val statusUpdater = actor {
    var running = true
    var inProgress = 0
    while (running || inProgress > 0) {
      receive {
        case Started => 
          inProgress += 1
        case InProgress(percent) => 
          println("Sprinkler at " + percent + "% done")
        case Complete => 
          println("Sprinkler completed")
          inProgress -= 1
        case Error(message) => 
          println("Sprinkler failed with error " + message)
          inProgress -= 1
        case Shutdown => 
          running = false
        case a =>
          println("statusUpdater received unknown message: " + a)
      }
    }
  }

  def register(sprinkler:Actor)   = sprinklers ::= sprinkler
  def unregister(sprinkler:Actor) = sprinklers -= sprinkler

  def send(photo:Photo) {
    println("sending photo to " + sprinklers.size + " sprinklers")
    sprinklers.foreach { s => s.send(photo, statusUpdater) }
  }

  def shutdown {
    println("shuting down " + sprinklers.size + " sprinklers")
    sprinklers.foreach { s => s ! Shutdown }
    sprinklers = List()
    statusUpdater ! Shutdown
  }

}