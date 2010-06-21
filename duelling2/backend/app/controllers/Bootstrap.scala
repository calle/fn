package imagesprinkler

import play.jobs._

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.sprinkler._


@OnApplicationStart
class Bootstrap extends Job {
  
  override def doJob() {
    val backend = new Backend()

//    new DebugSprinkler(backend).start
//    new SlowSprinkler(backend).start
    new ExternalJsonSprinkler(backend).start

    controllers.Sprinkler.backend = Some(backend)
    println("Bootstrap completed")
  }

}