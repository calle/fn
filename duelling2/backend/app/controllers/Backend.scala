package imagesprinkler

import play.jobs._

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.sprinkler._
import controllers.sprinkler.ExternalJsonSprinkler

@OnApplicationStart
class Bootstrap extends Job {
  
  override def doJob() {
    val backend = new Backend()

    new DebugSprinkler(backend).start
    new SlowSprinkler(backend).start
    new ExternalJsonSprinkler(backend).start

    controllers.Sprinkler.backend = Some(backend)
    println("Bootstrap completed")
  }

}

class Backend {

  var sprinklers = List[Actor]()

  val statusUpdater = actor {
    var running = true
    var inProgress = 0

    while (running || inProgress > 0) {
      receive {
        case Send(photo) => 
          println("Photo will be send to sprinklers")
        case Shutdown => 
          println("Shutting down statusUpdater")
          running = false

        case Started(sprinkler, photo) => 
          inProgress += 1
          println("Sprinkler \"" + sprinkler.name + "\" started")
        case InProgress(sprinkler, photo, percent) => 
          println("Sprinkler \"" + sprinkler.name + "\" is at " + percent + "% done")
        case Complete(sprinkler, photo) => 
          println("Sprinkler \"" + sprinkler.name + "\" completed")
          inProgress -= 1
        case Error(sprinkler, photo, message) => 
          println("Sprinkler \"" + sprinkler.name + "\" failed with error " + message)
          inProgress -= 1
      }
    }
  }

  def register(sprinkler:Actor)   = sprinklers ::= sprinkler
  def unregister(sprinkler:Actor) = sprinklers = sprinklers.filterNot(_ == sprinkler)

  def send(photo:Photo) {
    println("Sending photo to " + sprinklers.size + " sprinklers")
    sprinklers.foreach { s => s.send(Send(photo), statusUpdater) }
  }

  def shutdown {
    println("Shuting down " + sprinklers.size + " sprinklers")
    sprinklers.foreach { s => s.send(Shutdown, statusUpdater) }
    sprinklers = List()
    statusUpdater ! Shutdown
  }

}