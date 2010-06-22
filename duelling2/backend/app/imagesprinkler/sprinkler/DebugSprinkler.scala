package imagesprinkler.sprinkler

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.Backend

class DebugSprinkler() extends Sprinkler {

  var name = "debug sprinkler"

  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          println("Debug sprinkler sending photo with title " + photo.title)
          reply(Started(SendInstance(this, this.name, photo)));
          println("Debug sprinkler sending completed")
          reply(Complete(SendInstance(this, this.name, photo)));
        }
        case Shutdown => {
          println("Debug sprinkler shutting down")
          return;
        }
      }
    }
  }

}
