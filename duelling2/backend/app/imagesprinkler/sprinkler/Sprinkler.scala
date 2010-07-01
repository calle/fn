package imagesprinkler.sprinkler

import scala.actors.Actor

abstract class Sprinkler extends Actor {
  def name:String
}
