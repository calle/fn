package imagesprinkler

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.TIMEOUT

import imagesprinkler.sprinkler._


case class Register(spinkler:Sprinkler)
case class Unregister(spinkler:Sprinkler)

class Backend extends Actor {

  val ShutdownTimeout = 10000

  case class BackendSprinkler(var photos:Map[Photo, Map[String, BackendStatus]]);
  case class BackendStatus(statuses:List[String]);

  var sprinklers = Map[Sprinkler, BackendSprinkler]()

  def act() {
    var running = true
    while (running || sprinklers.size > 0) {
      receiveWithin(ShutdownTimeout) {
        case Register(sprinkler) =>
          sprinklers = sprinklers.updated(sprinkler, BackendSprinkler(Map()))
        case Unregister(sprinkler) =>
          sprinklers = sprinklers.filterNot(_ == sprinkler)

        case Send(photo) => 
          println("Sending photo to " + sprinklers.size + " sprinklers")
          sprinklers.foreach { s => 
            val (sprinkler, info) = s;
            info.photos = info.photos.updated(photo, Map())
            sprinkler ! Send(photo)
          }

        case Shutdown => 
          println("Shuting down " + sprinklers.size + " sprinklers")
          sprinklers.foreach { s => s._1 ! Shutdown }
          running = false

        case TIMEOUT if !running =>
          println("Timed out waiting for Shutdown of sprinklers, exiting")
          sprinklers = Map()
          
        case Started(SendInstance(sprinkler, key, photo)) => 
          println("Received Start from " + sprinkler + "@" + key + " for photo: " + photo)
          for {
            s <- sprinklers.get(sprinkler)
            p <- s.photos.get(photo)
          } {
            s.photos = s.photos.updated(photo, Map(key -> List("Started sprinkler " + sprinkler.name + "." + sprinkler.key)))
          }

        case InProgress(SendInstance(sprinkler, key, photo), message) => 
          println("Received InProgress from " + sprinkler + "@" + key + " for photo: " + photo)
          for {
            sprinkler <- sprinklers.get(sprinkler)
            photo <- sprinkler.photos.get(photo)
            statuses <- photo.get(key)
          } {
            statuses ::= message
          }

        case Complete(SendInstance(sprinkler, key, photo)) => 
          println("Received Complete from " + sprinkler + "@" + key + " for photo: " + photo)
          for {
            sprinkler <- sprinklers.get(spinkler)
            photo <- sprinkler.photos.get(photos)
            statuses <- photo.get(key)
          } {
            statuses ::= "Completed"
          }

        case Error(SendInstance(sprinkler, key, photo), message) => 
          println("Received Error from " + sprinkler + "@" + key + " for photo: " + photo)
          for {
            sprinkler <- sprinklers.get(spinkler)
            photo <- sprinkler.photos.get(photos)
            statuses <- photo.get(key)
          } {
            statuses ::= "Error: " + message
          }
      }
    }
  }

}