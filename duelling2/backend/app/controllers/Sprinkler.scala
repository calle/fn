package controllers

import play._
import play.mvc._
import java.io.File
import java.util.UUID

import imagesprinkler.{Backend, Photo, ImageFile}
import imagesprinkler.sprinkler.{Send, Shutdown}


object Sprinkler extends Controller {

  var backend : Option[Backend] = None

  def index = render()

  def test() {
    val id = UUID.randomUUID.toString
    val photo = Photo(id, "title", "description", new ImageFile(new File("public/images/fn.jpg")))
    println("sending photo: " + photo)
    backend.map( _ ! Send(photo) )
    renderText(id)
  }

  def send(title:String, description:String, file:File) {
    val id = UUID.randomUUID.toString
    val photo = Photo(id, title, description, new ImageFile(file))
    println("sending photo: " + photo)
    backend.map( _ ! Send(photo) )
    renderText(id)
  }

  def status(uuid: String) {
    renderText("status for " + uuid)
  }

  def shutdown() {
    println("sending shutdown")
    backend.map( _ ! Shutdown )
  }

}
