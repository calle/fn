package imagesprinkler.sprinkler

import imagesprinkler.Photo
import scala.actors.Actor

abstract class Sprinkler extends Actor {
  def name:String
}

abstract class Request
case class  Send(photo:Photo) extends Request
case object Shutdown extends Request

case class PhotoInstance(photo:Photo, key:String)

abstract class Response
case class Started(sprinkler:Sprinkler, instance:PhotoInstance) extends Response
case class InProgress(sprinkler:Sprinkler, instance:PhotoInstance, message:String) extends Response
case class Error(sprinkler:Sprinkler, instance:PhotoInstance, message:String) extends Response
case class Complete(sprinkler:Sprinkler, instance:PhotoInstance) extends Response
