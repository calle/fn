package controllers

import play._
import play.mvc._
import java.io.File
import java.util.UUID

import imagesprinkler.{Backend, Photo, ImageFile}
import imagesprinkler.sprinkler.{Send, Shutdown}


object Input extends Controller {

  var backend : Option[Backend] = None

  def index = render()

  def test() {
    val uuid = UUID.randomUUID.toString
    val photo = Photo(uuid, "title", "description", new ImageFile(new File("public/images/fn.jpg")))
    println("sending photo: " + photo)
    backend.map( _ ! Send(photo) )
    completed(uuid)
  }

  def send(title:String, description:String, file:File) {
    val uuid = UUID.randomUUID.toString
    val photo = Photo(uuid, title, description, new ImageFile(file))
    println("sending photo: " + photo)
    backend.map( _ ! Send(photo) )
    completed(uuid)
  }

  def completed(uuid:String) {
	render(uuid)
  }

  def status(uuid: String) {
    backend match {
    	case Some(b) => b !? Backend.GetStatus(uuid) match {
    		case Backend.GetStatusResponse(statuses) => {
    			renderText(statuses.map { 
    				case (key, statuses) => "\"" + key + "\": " + statuses.map { "\"" + _ + "\"" }.mkString("[", ",", "]")
    			}.mkString("{", ",", "}"))
    		}
    		case _ => renderText("{ error:\"Unknown uuid: " + uuid + "\"}");
    	}
    	case _ => renderText("{ error:\"Not started correctly\"}");
    }
  }

  def shutdown() {
    println("sending shutdown")
    backend.map( _ ! Shutdown )
  }

}
