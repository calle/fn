package controllers

import play.jobs._

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler.{Backend, Register}
import imagesprinkler.sprinkler._


@OnApplicationStart
class Bootstrap extends Job {

  override def doJob() {
    val backend = new Backend()

    // Start backend
    backend.start()

    // Tell the REST interface about our existance
    controllers.Sprinkler.backend = Some(backend)

    /**
     * Register default sprinklers
     */
    register(backend, new DebugSprinkler())
//    register(new SlowSprinkler())
    register(backend, new ExternalJsonSprinkler())

    // Done!
    println("Bootstrap completed")
  }

  private def register(backend:Backend, sprinkler:Sprinkler) {
    backend ! Register(sprinkler)
    sprinkler.start
  }

}