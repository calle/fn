package se.netlight.fn.imagesprinkler

import scala.actors.Actor
import scala.actors.Actor._

abstract class Request
case class Photo(title:String, description:String, image:java.awt.Image) extends Request
case object Shutdown extends Request

abstract class Response
case object Started extends Response
case class  InProgress(percent:Int) extends Response
case class  Error(message:String) extends Response
case object Complete extends Response

class DebugSprinkler(backend:Backend) extends Actor {
  backend.register(this);

  def act() {
    while (true) {
      receive {
        case Photo(title, description, image) =>
          reply(Started)
          println("Received image with title " + title)
          reply(InProgress(20))
          Thread.sleep(1000)
          println("Send reply with 60%")
          reply(InProgress(60))
          Thread.sleep(1800)
          println("Send complete")
          reply(Complete)
        case Shutdown => 
          println("Shuting down sprinkler")
          return;
      }
    }
  }
}