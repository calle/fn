package controllers

import play._
import play.mvc._
import play.libs.WS
import java.io.File
import java.util.UUID

import imagesprinkler._
import imagesprinkler.listener.InstanceListener

object Input extends Controller {

  var backend : Option[Backend] = None

  def index = render()

  def test() {
    val uuid = UUID.randomUUID.toString
    val photo = Photo(uuid, "title", "description", new ImageFile(new File("public/images/fn.jpg")))
    println("sending photo: " + photo)
    backend.map( _ ! Send(photo) )
    flash.success("Send photo with id %s", uuid);
    index
  }

  def send(title:String, description:String, file:File) {
    val uuid = UUID.randomUUID.toString
    val photo = Photo(uuid, title, description, new ImageFile(file))
    println("sending photo: " + photo)
    backend.map( _ ! Send(photo) )
    flash.success("Send photo with id %s", uuid);
    index
  }

  def sendWithCallback(title:String, description:String, image:String, callback:String) {
    val uuid = UUID.randomUUID.toString
    val photo = Photo(uuid, title, description, new ImageBase64(image))
    println("sending photo: " + photo + " with callback: " + callback)
    // Hack to allow for parameter inside block
    val theCallback = callback
    backend.map { b => 
      val listener = new CallbackListener(b, photo.id, theCallback)
      listener.send(photo)
    }
    renderText("uuid: " + uuid)
  }

  def shutdown() {
    println("sending shutdown")
    backend.map( _ ! Shutdown )
  }

  private class CallbackListener(b:Backend, id:String, url:String) extends InstanceListener(b) {

    def started(status:Started) {
      send(status.instance, "started")
    }
    def inProgress(status:InProgress) {
      send(status.instance, status.message)
    }
    def complete(status:Complete) {
      send(status.instance, "complete")
    }
    def error(status:Error) {
      send(status.instance, "error: " + status.message)
    }

    def send(instance:PhotoInstance, status:String) {
      try {
        val id = instance.photo.id
        val request = WS.url(url)
          .setParameter("id", id)
          .setParameter("status", status)
        request.timeout = 10000;

        println("Sending status update for " + id + " to " + url)
        println(request);
        request.post();
        println("Sent status update for " + id + " to " + url)
      } catch { 
        case e => 
          println("Failed sending status update for " + id + " to " + url + ": " + e.getMessage())
      }
    }
  }

}
