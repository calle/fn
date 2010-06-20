package imagesprinkler.sprinkler

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.Backend

class DebugSprinkler(backend:Backend) extends Sprinkler with Actor {

  var name = "debug sprinkler"

  backend.register(this)

  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          println("Debug sprinkler sending photo with title " + photo.title)
          reply(Started(this, photo));
          println("Debug sprinkler sending completed")
          reply(Complete(this, photo));
        }
        case Shutdown => {
          println("Debug sprinkler shutting down")
          return;
        }
      }
    }
  }

}
