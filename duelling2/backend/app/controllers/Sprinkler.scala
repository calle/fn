package controllers

import play._
import play.mvc._

object Sprinkler extends Controller {

    def send {
      renderText("uuid")
    }

    def status(uuid: String) {
      renderText("status for " + uuid)
    }

}
