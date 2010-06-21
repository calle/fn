package imagesprinkler.sprinkler

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.Backend

class SlowSprinkler(backend:Backend) extends Sprinkler with Actor {

  var name = "slow sprinkler"

  backend.register(this)

  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          println("Slow sprinkler send photo " + photo)
          reply(Started(this, photo));

          val steps = 10

          val range = 1.to(steps)
          for (i <- range) {
            Thread.sleep(2000)
            val percent = (100*(i/steps.toDouble)).toInt
            reply(InProgress(this, photo, "Done " + percent + "%"));
          }

          println("Slow sprinkler completed photo" + photo)
          reply(Complete(this, photo));
        }
        case Shutdown => {
          println("Slow sprinkler shutting down")
          return;
        }
      }
    }
  }

}
