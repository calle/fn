package controllers

import play._
import play.mvc._

import imagesprinkler.sprinkler._


object ExternalJsonController extends Controller {

  var sprinkler:Option[ExternalJsonSprinkler] = None

  def index = render()

  def register(name:String, url:String) {
	send(ExternalJsonSprinkler.Register(name, url))
	flash.success("Registered sprinkler %s", name);
    index
  }

  def unregister(name:String) { 
    send(ExternalJsonSprinkler.Unregister(name))
	flash.success("Unregistered sprinkler %s", name);
    index
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

  private def send(message:Any) {
	sprinkler.map { s => s ! message }
  }
  
}
