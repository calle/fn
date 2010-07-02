package imagesprinkler.sprinkler


import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.OutputChannel
import scala.collection.mutable.{ Buffer => MBuffer }

import imagesprinkler._

object SimpleSprinkler {
	case class GetPhoto()
	case class GetNextPhoto()
	case class GetPhotoResult(photo:Option[Photo])
}

class SimpleSprinkler() extends Sprinkler {

  var name = "simple sprinkler"

  private var photo : Option[Photo] = None
  private val awaitingPhoto : MBuffer[OutputChannel[Any]] = MBuffer()
  
  def act() {
    while (true) {
      receive {
        case Send(photo) => {
          reply(Started(this, PhotoInstance(photo, this.name)));
          this.photo = Some(photo)
          awaitingPhoto.foreach { actor => actor ! SimpleSprinkler.GetPhotoResult(Some(photo)) }
          awaitingPhoto.clear
          reply(Complete(this, PhotoInstance(photo, this.name)));
        }
        case Shutdown => {
          this.photo = None
          return;
        }
        case SimpleSprinkler.GetPhoto => {
          reply(SimpleSprinkler.GetPhotoResult(photo))
        }
        case SimpleSprinkler.GetNextPhoto => {
          awaitingPhoto.append(sender)
        }
      }
    }
  }

}
