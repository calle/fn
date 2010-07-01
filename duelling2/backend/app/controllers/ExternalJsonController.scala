package controllers

import play._
import play.mvc._

import imagesprinkler.sprinkler._


object ExternalJsonController extends Controller {

  var sprinkler:Option[ExternalJsonSprinkler] = None

  def index = render()

  def register(name:String, url:String) {
	send(ExternalJsonSprinkler.Register(name, url))
  }

  def status(name:String, id:String, message:String) {
	send(ExternalJsonSprinkler.Status(name, id, message))
  }

  def done(name:String, id:String) {
    send(ExternalJsonSprinkler.Done(name, id))
  }

  def error(name:String, id:String, message:String) { 
    send(ExternalJsonSprinkler.Error(name, id, message))
  }

  def unregister(name:String) { 
    send(ExternalJsonSprinkler.Unregister(name))
  }

  private def send(message:Any) {
	sprinkler.map { s => s ! message }
  }
  
}
