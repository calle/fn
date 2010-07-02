package imagesprinkler

import imagesprinkler.sprinkler.Sprinkler

case class PhotoInstance(photo:Photo, key:String)

abstract class Response
case class Started(sprinkler:Sprinkler, instance:PhotoInstance) extends Response
case class InProgress(sprinkler:Sprinkler, instance:PhotoInstance, message:String) extends Response
case class Error(sprinkler:Sprinkler, instance:PhotoInstance, message:String) extends Response
case class Complete(sprinkler:Sprinkler, instance:PhotoInstance) extends Response
