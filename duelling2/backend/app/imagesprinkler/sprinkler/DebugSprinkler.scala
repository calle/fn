package imagesprinkler.sprinkler

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler._


class DebugSprinkler() extends Sprinkler {

  var name = "debug sprinkler"

  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          println("Debug sprinkler sending photo with title " + photo.title)
          reply(Started(this, PhotoInstance(photo, this.name)));
          println("Debug sprinkler sending completed")
          reply(Complete(this, PhotoInstance(photo, this.name)));
        }
        case Shutdown => {
          println("Debug sprinkler shutting down")
          return;
        }
      }
    }
  }

}
