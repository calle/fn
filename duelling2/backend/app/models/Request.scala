package imagesprinkler

abstract class Request
case class  Send(photo:Photo) extends Request
case object Shutdown extends Request
