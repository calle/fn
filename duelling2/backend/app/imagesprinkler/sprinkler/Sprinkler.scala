package imagesprinkler.sprinkler

import imagesprinkler.Photo

abstract class Sprinkler {
  def name:String
}

abstract class Request
case class  Send(photo:Photo) extends Request
case object Shutdown extends Request

abstract class Response
case class Started(sprinkler:Sprinkler, photo:Photo) extends Response
case class InProgress(sprinkler:Sprinkler, photo:Photo, message:String) extends Response
case class Error(sprinkler:Sprinkler, photo:Photo, message:String) extends Response
case class Complete(sprinkler:Sprinkler, photo:Photo) extends Response
