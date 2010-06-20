package controllers.sprinkler

import play._
import play.mvc._
import play.jobs._
import play.libs.WS
import scala.actors.OutputChannel
import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.Backend
import imagesprinkler._
import imagesprinkler.sprinkler._

object ExternalJson extends Controller {

  var sprinkler:Option[ExternalJsonSprinkler] = None

  def index = render()

  def register(name:String, url:String) {
    println("ExternalJsonController.register(" + name + ", " + url + ")")
    // sprinkler.map { s => s.send(ExtRegister(name, url), null) }
    println("done")
    // sprinkler.map { s => s ! ExtRegister(ffff, url) }
  }

  def status(name:String, id:String, message:String) =
    sprinkler.map { s => s ! ExtStatus(name, id, message) }

  def done(name:String, id:String) =
    sprinkler.map { s => s ! ExtDone(name, id) }

  def error(name:String, id:String, message:String) = 
    sprinkler.map { s => s ! ExtError(name, id, message) }

  def unregister(name:String) = 
    sprinkler.map { s => s ! ExtUnregister(name) }

}

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

    while (running || sprinklers.size > 0) {
      receive {
        case Send(photo) =>
          val source = sender
          sprinklers.mapValues { s => 
            s.photos = s.photos.updated(photo.id, ExtPhotoInProgress(photo, source))
            s.actor ! Send(photo)
          }

        case Shutdown => 
          sprinklers.mapValues { s => s.actor ! Shutdown}

        // Internal methods
        case ExtRegister(name, url) => 
          println("Registering " + name)
          sprinklers = sprinklers + (name -> ExtSprinkler(new ExternalJsonActor(name, url), Map()))

        case ExtUnregister(name) => 
          println("UNregistering " + name)
          val (keep, removed) = sprinklers.partition { _._1 == name }
          removed.mapValues { s => s.actor ! Shutdown }
          sprinklers = keep

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
          }
          self ! ExtUnregister(name)

        case ExtError(name, id, message) => 
          println("Error sending to " + name + " of " + id)
          for {
            sprinkler <- sprinklers.get(name)
            photo <- sprinkler.photos.get(id)
          } {
            photo.source ! imagesprinkler.sprinkler.Error(this, photo.photo, message)
          }
          self ! ExtUnregister(name)

        case message => 
          println("ERROR: ExternalJsonSprinkler received unknown message: " + message)

      }
    }
    println("ExternalJsonSprinkler shutting down")
  }

}


class ExternalJsonActor(name: String, url: String) extends Actor {

  def act() {
    var running = true
    while (running) {
      receive {
        case Send(photo) => 
          sender ! ExtStart(name, photo.id)

          val request = WS.url(url)
            .setParameter("id", photo.id)
            .setParameter("title", photo.title)
            .setParameter("description", photo.title)
            .setParameter("image", photo.image.asBase64)
            .files(photo.image.asFile)
            .timeout(10);
          
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
          
        case Shutdown => 
          println("Shutting down external application towards " + url)
          running = false
      }
    }
    println("Terminating external application towards " + url)
  } 

}