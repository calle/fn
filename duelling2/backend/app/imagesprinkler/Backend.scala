package imagesprinkler

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.TIMEOUT
import scala.collection.mutable.{ Map => MMap, Buffer => MBuffer }

import imagesprinkler.sprinkler._


object Backend {
  val ShutdownTimeout = 10000

  case class Register(spinkler:Sprinkler)
  case class Unregister(spinkler:Sprinkler)
  case class GetStatus(uuid:String)
  case class GetStatusResponse(statuses:Map[String, List[String]])

  private case class SprinklerInfo(var photos:MMap[PhotoInstance, StatusList]) {
	def addPhoto(instance:PhotoInstance) {
	  photos.update(instance, Backend.StatusList(MBuffer()))
	}
	def addStatus(instance:PhotoInstance, status:String) {
	  photos.get(instance).foreach { _.statuses.append(status) }
	}
  }
  private case class StatusList(statuses:MBuffer[String]) 
}

class Backend extends Actor {

  var sprinklers = MMap[Sprinkler, Backend.SprinklerInfo]()

  def act() {
    var running = true

    while (running || sprinklers.size > 0) {

      receiveWithin(Backend.ShutdownTimeout) {

        case Backend.Register(sprinkler) =>
          sprinklers.update(sprinkler, Backend.SprinklerInfo(MMap()))

        case Backend.Unregister(sprinkler) =>
          sprinklers.remove(sprinkler)

        case Backend.GetStatus(uuid) =>
          // Iterate over sprinklers to find all photo instances matching uuid
          // TODO: Keep cache from uuid -> StatusList instances maybe
          var response = Map[String, List[String]]()
          sprinklers.foreach { 
        	case (_, info) => {
			  info.photos.foreach { 
				case (instance, statusList) if instance.photo.id == uuid => 
					// Append to response map
					response += (instance.key -> List(statusList.statuses : _*))
				case _ => 
			  }
        	}
          }
          reply(Backend.GetStatusResponse(response))

        case Send(photo) => 
          println("Sending photo to " + sprinklers.size + " sprinklers")
          sprinklers.keys.foreach { sprinkler => sprinkler ! Send(photo) }

        case Shutdown => 
          println("Shuting down " + sprinklers.size + " sprinklers")
          sprinklers.keys.foreach { s => s ! Shutdown }
          running = false

        case TIMEOUT if !running =>
          println("Timed out waiting for Shutdown of sprinklers, exiting")
          sprinklers.clear
          
        case Started(sprinkler, instance @ PhotoInstance(photo, key)) => 
          println("Received Start from " + sprinkler + " for photo: " + photo + "@" + key)
          sprinklers.get(sprinkler).foreach { s =>
          	s.addPhoto(instance)
            s.addStatus(instance, "Sprinkler " + sprinkler.name + " started " + photo.id + "." + key)
          }
          // TODO: Remove photo after a longer while...

        case InProgress(sprinkler, instance @ PhotoInstance(photo, key), message) => 
          println("Received InProgress from " + sprinkler + " for photo: " + photo + "@" + key)
          sprinklers.get(sprinkler).foreach { _.addStatus(instance, message) }
          // TODO: Refresh removal time for photo...

        case Complete(sprinkler, instance @ PhotoInstance(photo, key)) => 
          println("Received Complete from " + sprinkler + " for photo: " + photo + "@" + key)
          sprinklers.get(sprinkler).foreach { _.addStatus(instance, "Completed") }
          // TODO: Remove photo after a while...

        case Error(sprinkler, instance @ PhotoInstance(photo, key), message) => 
          println("Received Error from " + sprinkler + " for photo: " + photo + "@" + key)
          sprinklers.get(sprinkler).foreach { _.addStatus(instance, "Failed: " + message) }
          // TODO: Remove photo after a while...

      }
    }
  }
  

}