package imagesprinkler.sprinkler

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.Backend

class SlowSprinkler() extends Sprinkler {

  var name = "slow sprinkler"

  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          println("Slow sprinkler send photo " + photo)
          val instance = SendInstance(this, this.name, photo)
          reply(Started(instance));

          val steps = 10

          val range = 1.to(steps)
          for (i <- range) {
            Thread.sleep(2000)
            val percent = (100*(i/steps.toDouble)).toInt
            reply(InProgress(instance, "Done " + percent + "%"));
          }

          println("Slow sprinkler completed photo" + photo)
          reply(Complete(instance));
        }
        case Shutdown => {
          println("Slow sprinkler shutting down")
          return;
        }
      }
    }
  }

}
