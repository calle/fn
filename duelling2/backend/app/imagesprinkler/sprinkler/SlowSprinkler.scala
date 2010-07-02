package imagesprinkler.sprinkler

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler._

class SlowSprinkler() extends Sprinkler {

  var name = "slow sprinkler"

  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          println("Slow sprinkler send photo " + photo)
          val instance = PhotoInstance(photo, this.name)
          reply(Started(this, instance));

          val steps = 10

          val range = 1.to(steps)
          for (i <- range) {
            Thread.sleep(2000)
            val percent = (100*(i/steps.toDouble)).toInt
            reply(InProgress(this, instance, "Done " + percent + "%"));
          }

          println("Slow sprinkler completed photo" + photo)
          reply(Complete(this, instance));
        }
        case Shutdown => {
          println("Slow sprinkler shutting down")
          return;
        }
      }
    }
  }

}
