package controllers

import play._
import play.mvc._

import imagesprinkler.sprinkler._


object ExternalJsonController extends Controller {

  var sprinkler:Option[ExternalJsonSprinkler] = None

  def index = render()

  def register(name:String, url:String) {
	send(ExtRegister(name, url))
  }

  def status(name:String, id:String, message:String) {
	send(ExtStatus(name, id, message))
  }

  def done(name:String, id:String) {
    send(ExtDone(name, id))
  }

  def error(name:String, id:String, message:String) { 
    send(ExtError(name, id, message))
  }

  def unregister(name:String) { 
    send(ExtUnregister(name))
  }

  private def send(message:Any) {
	sprinkler.map { s => s ! message }
  }
  
}
