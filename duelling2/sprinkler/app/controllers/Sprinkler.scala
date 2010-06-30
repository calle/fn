package controllers

import play._
import play.mvc._
import play.libs.WS
import java.io.File

object Sprinkler extends Controller {

  val root = "http://localhost:9000/sprinkler/ext"

  def index = render()

  def register {
    println("Registering to " + root + "/register with url " + Router.getFullUrl("Sprinkler.send"))
    
    val result = WS.url(root + "/register")
      .setParameter("name", "testing")
      .setParameter("url", Router.getFullUrl("Sprinkler.send"))
      .post()

    renderText("registered, status: " + result.getStatus)
  }

  def send(id:String, title:String, description:String, file:File) {
    println("Received sending of " + id)

    // Make a slow handling with statuses
    WS.url(root + "/status")
      .setParameter("name", "testing")
      .setParameter("id", id)
      .setParameter("message", "starting")
      .post()

    Thread.sleep(2000)

    WS.url(root + "/status")
      .setParameter("name", "testing")
      .setParameter("id", id)
      .setParameter("message", "after 2s")
      .post()

    Thread.sleep(3000)

    WS.url(root + "/status")
      .setParameter("name", "testing")
      .setParameter("id", id)
      .setParameter("message", "after 5s")
      .post()

    Thread.sleep(1000)

    WS.url(root + "/done")
      .setParameter("name", "testing")
      .setParameter("id", id)
      .post()
  }

  def unregister {
    WS.url(root + "/unregister")
      .setParameter("name", "testing")
      .post();
    renderText("ok")
  }

}
