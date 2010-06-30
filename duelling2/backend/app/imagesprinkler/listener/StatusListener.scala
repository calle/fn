package imagesprinkler.listener

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.{ Map => MMap, Buffer => MBuffer }

import imagesprinkler._
import imagesprinkler.sprinkler._

object StatusListener {
  case class GetStatus(uuid:String)
  case class GetStatusResponse(statuses:Map[String, List[Response]])
}

class StatusListener extends Listener {

  val photos = MMap[PhotoInstance, MBuffer[Response]]()

  def act() {
    var running = true
    while (running) {
      receive {

        case StatusListener.GetStatus(uuid) =>
          // Iterate over sprinklers to find all photo instances matching uuid
          // TODO: Keep cache from uuid -> StatusList instances maybe
          var response = Map[String, List[Response]]()
          photos.foreach { 
            case (instance, statuses) if instance.photo.id == uuid => 
              // Append to response map
              response += (instance.key -> List(statuses : _*));
            case _ => 
          }
          reply(StatusListener.GetStatusResponse(response))

        case Shutdown => {
          println("Shuting down StatusListener")
          running = false
        }

        /**
         * TODO: Remove photos after a while¨
         */

        case status @ Started(sprinkler, instance @ PhotoInstance(photo, key)) => 
          println("Received Start from " + sprinkler + " for photo: " + photo + "@" + key)
          photos(instance) = MBuffer(status)

        case status @ InProgress(sprinkler, instance @ PhotoInstance(photo, key), message) => 
          println("Received InProgress from " + sprinkler + " for photo: " + photo + "@" + key)
          photos.get(instance).get += status

        case status @ Complete(sprinkler, instance @ PhotoInstance(photo, key)) => 
          println("Received Complete from " + sprinkler + " for photo: " + photo + "@" + key)
          photos.get(instance).get += status

        case status @ Error(sprinkler, instance @ PhotoInstance(photo, key), message) => 
          println("Received Error from " + sprinkler + " for photo: " + photo + "@" + key)
          photos.get(instance).get += status

      }
    }
  }
  
}