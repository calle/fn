package controllers

import play.jobs._

import scala.actors.Actor
import scala.actors.Actor._

import imagesprinkler._
import imagesprinkler.sprinkler._
import imagesprinkler.listener._

@OnApplicationStart
class Bootstrap extends Job {

  override def doJob() {
    val backend = new Backend()

    // Start backend
    backend.start()

    // Tell the REST interface about our existance
    Input.backend = Some(backend)
    StatusController.backend = Some(backend)

    /**
     * Register sprinklers
     */

/*
    register(backend, new DebugSprinkler());

    val simple = new SimpleSprinkler()
    register(backend, simple);
    SimpleSprinklerController.sprinkler = Some(simple)
*/

    val external = new ExternalJsonSprinkler(backend)
    register(backend, external);
    ExternalJsonController.sprinkler = Some(external)

    // Done!
    println("Bootstrap completed")
  }

  private def register(backend:Backend, sprinkler:Sprinkler) {
    backend ! Backend.RegisterSprinkler(sprinkler)
    sprinkler.start
  }

  private def register(backend:Backend, listener:Listener) {
    backend ! Backend.RegisterListener(listener)
    listener.start
  }

}