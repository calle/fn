package imagesprinkler.sprinkler

import play.libs.WS
import scala.actors.OutputChannel
import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.Backend
import imagesprinkler._
import controllers.ExternalJson

case class ExtRegister(name:String, url:String)
case class ExtUnregister(name:String)

case class ExtStart(name:String, id:String)
case class ExtStatus(name:String, id:String, message:String)
case class ExtDone(name:String, id:String)
case class ExtError(name:String, id:String, message:String)

case class ExtSprinkler(actor:Actor, var photos:Map[String, ExtPhotoInProgress]);
case class ExtPhotoInProgress(photo:Photo, source:OutputChannel[Any]);

class ExternalJsonSprinkler(backend:Backend) extends Sprinkler with Actor {

  var name = "external json sprinkler"
  var sprinklers = Map[String, ExtSprinkler]()

  ExternalJson.sprinkler = Some(this)

  backend.register(this);

  def act() {
    var running = true

    try {
    while (running || sprinklers.size > 0) {
      receive {
        case Send(photo) =>
          println("Sending photo to external sprinklers " + sprinklers.size)
          val source = sender
          sprinklers.foreach { item => val (name, s) = item
            s.photos = s.photos.updated(photo.id, ExtPhotoInProgress(photo, source))
            println("Sending photo to external sprinkler " + s)
            s.actor ! Send(photo)
          }

        case Shutdown => 
          sprinklers.mapValues { s => s.actor ! Shutdown}

        // Internal methods
        case ExtRegister(name, url) => 
          println("Registering " + name)
          val actor = new ExternalJsonActor(name, url)
          actor.start
          sprinklers = sprinklers + (name -> ExtSprinkler(actor, Map()))

        case ExtUnregister(name) => 
          println("Unregistering " + name)
          println("Before: " + sprinklers)
          val (removed, keep) = sprinklers.partition { _._1 == name }
          removed.foreach { item => val (name, s) = item; s.actor ! Shutdown }
          sprinklers = keep
          println("After: " + sprinklers)

        case ExtStart(name, id) => 
          println("Started sending to " + name + " with " + id)
          for {
            sprinkler <- sprinklers.get(name)
            photo <- sprinkler.photos.get(id)
          } {
            photo.source ! imagesprinkler.sprinkler.Started(this, photo.photo)
          }

        case ExtStatus(name, id, message) => 
          println("Got status from " + name + " for " + id)
          for {
            sprinkler <- sprinklers.get(name)
            photo <- sprinkler.photos.get(id)
          } {
            photo.source ! imagesprinkler.sprinkler.InProgress(this, photo.photo, message)
          }

        case ExtDone(name, id) => 
          println("Completed sending to " + name + " of " + id)
          for {
            sprinkler <- sprinklers.get(name)
            photo <- sprinkler.photos.get(id)
          } {
            photo.source ! imagesprinkler.sprinkler.Complete(this, photo.photo)
            sprinkler.photos -= id
          }

        case ExtError(name, id, message) => 
          println("Error sending to " + name + " of " + id)
          for {
            sprinkler <- sprinklers.get(name)
            photo <- sprinkler.photos.get(id)
          } {
            photo.source ! imagesprinkler.sprinkler.Error(this, photo.photo, message)
            sprinkler.photos -= id
          }

        case message => 
          println("ERROR: ExternalJsonSprinkler received unknown message: " + message)

      }
    }
    } catch {
    	case e => println("Execption while receiving message: " + e)
    }
    println("ExternalJsonSprinkler shutting down")
  }

}


class ExternalJsonActor(name: String, url: String) extends Actor {

  def act() {
	println("ExternalJsonActor started for " + name)
    var running = true
    while (running) {
      receive {
        case Send(photo) => 
          println("ExternalJsonActor received photo " + photo)

          sender ! ExtStart(name, photo.id)

          try {
	          val request = WS.url(url)
	            .setParameter("id", photo.id)
	            .setParameter("title", photo.title)
	            .setParameter("description", photo.title)
	            .setParameter("image", photo.image.asBase64)
	            .files(photo.image.asFile)
	            .timeout(10000);
	          
	          sender ! ExtStatus(name, photo.id, "Sending photo " + photo.id + " to " + url)
	          println("Sending photo " + photo.id + " to " + url)
	          val result = request.post();
	          println("Sent photo " + photo.id + " to " + url)
	          sender ! ExtStatus(name, photo.id, "Photo " + photo.id + " sent to " + url)
	
	          if (result.getStatus() != 200) {
	            sender ! ExtError(name, photo.id, "Failed to send photo to " + url + ", status = " + result.getStatus())
	          } else {
	            sender ! ExtDone(name, photo.id)
	          }
          } catch { 
        	  case e => 
        	   sender ! ExtError(name, photo.id, "Failed to send photo to " + url + ", exception " + e.getMessage())
          }
        case Shutdown => 
          println("Shutting down external application towards " + url)
          running = false
        
        case message =>
          println("ExternalJsonActor received unknown message: " + message)
      }
    }
    println("Terminating external application towards " + url)
  } 

}