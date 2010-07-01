package imagesprinkler.sprinkler

import play.libs.WS
import play.libs.WS.HttpResponse
import scala.actors.OutputChannel
import scala.actors.TIMEOUT
import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.{ Map => MMap }
import java.util.concurrent.Future

import imagesprinkler._


object ExternalJsonSprinkler {

	val ShutdownTimeout = Backend.ShutdownTimeout / 2
	
	case class Register(name:String, url:String)
	case class Unregister(name:String)
	
	case class Status(name:String, id:String, message:String)
	case class Done(name:String, id:String)
	case class Error(name:String, id:String, message:String)

}

class ExternalJsonSprinkler(backend:Backend) extends Sprinkler {

  val name = "external json sprinkler"
  val sprinklers = MMap[String, Sprinkler]()

  def act() {
    var running = true
    while (running || sprinklers.size > 0) {
      
      receiveWithin(ExternalJsonSprinkler.ShutdownTimeout) {

      	case ExternalJsonSprinkler.Register(name, url) =>
          println("Registering external sprinkler " + name)
          val sprinkler = new ExternalJsonInstanceSprinkler(backend, name, url)
          sprinklers += (name -> sprinkler)
          sprinkler.start

        case ExternalJsonSprinkler.Unregister(name) => 
          println("Unregistering external sprinkler " + name)
          sprinklers.remove(name).map ( _ ! Shutdown )

        case Send(photo) =>
          // Ignore sent photos, sprinklers are registered themselves and will receive photos

        case Shutdown => 
          sprinklers.foreach { case (name, s) => s ! Shutdown }
          running = false

        case TIMEOUT => 
          if (!running) {
            println("Timed out waiting for Shutdown of sprinklers, exiting")
            sprinklers.clear
          }
          
        case ExternalJsonSprinkler.Status(name, id, message) => 
          println("Got status from " + name + " for " + id)
          sprinklers.get(name).map ( _ ! ExternalJsonInstanceSprinkler.Status(id, message) )

        case ExternalJsonSprinkler.Done(name, id) => 
          println("Completed sending to " + name + " of " + id)
          sprinklers.get(name).map ( _ ! ExternalJsonInstanceSprinkler.Done(id) )

        case ExternalJsonSprinkler.Error(name, id, message) => 
          println("Error sending to " + name + " of " + id)
          sprinklers.get(name).map ( _ ! ExternalJsonInstanceSprinkler.Error(id, message) )

        case message => 
          println("ERROR: ExternalJsonSprinkler received unknown message: " + message)

      }
    }

    println("ExternalJsonSprinkler shutting down")
  }

}

private object ExternalJsonInstanceSprinkler {

	case class PhotoInfo(sprinkler:ExternalJsonInstanceSprinkler, photo:Photo, source:OutputChannel[Any], var request:Option[Future[HttpResponse]]) {
    def sendStart() = source ! imagesprinkler.Started(sprinkler, toInstance())
	  def sendStatus(message:String) = source ! imagesprinkler.InProgress(sprinkler, toInstance(), message)
    def sendDone() = source ! imagesprinkler.Complete(sprinkler, toInstance())
    def sendError(message:String) = source ! imagesprinkler.Error(sprinkler, toInstance(), message)
    private def toInstance() = PhotoInstance(photo, sprinkler.name)
	}

	case class Start(id:String)
	case class Status(id:String, message:String)
	case class Done(id:String)
	case class Error(id:String, message:String)

	val CheckAsyncRequestsInterval = 1000
	
}


private class ExternalJsonInstanceSprinkler(backend:Backend, sprinklerName: String, url: String) extends Sprinkler {

  import ExternalJsonInstanceSprinkler.PhotoInfo

  def name = "external." + sprinklerName
  
  val photos = MMap[String, PhotoInfo]()

  def act() {
    println("ExternalJsonActor started for " + sprinklerName)

    backend ! Backend.RegisterSprinkler(this)

    var running = true
    while (running) {
      
      receiveWithin(ExternalJsonInstanceSprinkler.CheckAsyncRequestsInterval) {
        case Send(photo) => 
          println("ExternalJsonActor received photo " + photo)
          val info = ExternalJsonInstanceSprinkler.PhotoInfo(this, photo, sender, None)
          photos += (photo.id -> info)
          info.sendStart
          val request = createRequest(photo)

          // Perform status update and start async request
          info.sendStatus("POST photo " + photo.id + " to " + url)
          info.request = Some(request.postAsync());

        case TIMEOUT =>
          photos.foreach { 
            case (id, info @ PhotoInfo(_, photo, _, Some(request))) if request.isDone => {
            	info.sendStatus("Photo " + photo.id + " sent to " + url)
            	val result = request.get()
            	if (result.getStatus() != 200) {
            		info.sendError("Failed to send photo to " + url + ", status = " + result.getStatus())
            	} else {
            		info.sendDone()
            	}
        			//    } catch { 
        			//      case e => 
        			//        reply(ExternalJsonInstanceSprinkler.Error(photo.id, "Failed to send photo to " + url + ", exception " + e.getMessage()))
        			//    }
            }
            case _ => 
          }

        case Shutdown => 
          println("ExternalJsonInstanceSprinkler " + sprinklerName + " shutting down towards " + url)
          photos.foreach { case (id, info) => info.sendError("Shutdown received") }
          photos.clear
          running = false

        case ExternalJsonInstanceSprinkler.Status(id, message) => 
          println("ExternalJsonInstanceSprinkler " + sprinklerName + " got status for " + id)
          photos.get(id).map { _.sendStatus(message) }

        case ExternalJsonInstanceSprinkler.Done(id) => 
          println("ExternalJsonInstanceSprinkler " + sprinklerName + " completed sending of " + id)
          photos.remove(id).map { _.sendDone() }  

        case ExternalJsonInstanceSprinkler.Error(id, message) => 
          println("ExternalJsonInstanceSprinkler " + sprinklerName + " failed sending of " + id)
          photos.remove(id).map { _.sendError(message) }  

        case message =>
          println("ExternalJsonInstanceSprinkler " + sprinklerName + " received unknown message: " + message)
      }
    }

    backend ! Backend.UnregisterSprinkler(this)

    println("ExternalJsonInstanceSprinkler " + sprinklerName + " terminating towards " + url)
  } 

  def createRequest(photo:Photo) = {
    // Write data to temporary file
    val file = new java.io.File("/tmp/image_" + photo.id)
    saveToFile(photo.image, file)

    // Create request
    val request = WS.url(url)
        .setParameter("id", photo.id)
        .setParameter("title", photo.title)
        .setParameter("description", photo.title)
        .setParameter("imageBase64", photo.image.asBase64)
        .files(new WS.FileParam(file, "img"));

    // Set request timeout
    request.timeout = 10000;

    request
  }

  def saveToFile(image:Image, file:java.io.File) {
	val base64 = image.asBase64
	val data = base64.indexOf(",") match {
      case index:Int if index >= 0 => base64.substring(index)
      case _ => base64
    }
    play.libs.IO.write(play.libs.Codec.decodeBASE64(data), file)
  }
  
}