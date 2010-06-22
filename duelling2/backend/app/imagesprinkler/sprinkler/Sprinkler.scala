package imagesprinkler.sprinkler

import imagesprinkler.Photo
import scala.actors.Actor

abstract class Sprinkler extends Actor {
  def name:String
}

abstract class Request
case class  Send(photo:Photo) extends Request
case object Shutdown extends Request

case class SendInstance(sprinkler:Sprinkler, key:String, photo:Photo)

abstract class Response
case class Started(instance:SendInstance) extends Response
case class InProgress(instance:SendInstance, message:String) extends Response
case class Error(instance:SendInstance, message:String) extends Response
case class Complete(instance:SendInstance) extends Response
