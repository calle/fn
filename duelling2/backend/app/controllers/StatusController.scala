package controllers

import play._
import play.mvc._

import imagesprinkler._


object StatusController extends Controller {

  var statusListener : Option[StatusListener] = None

  def status(uuid: String) {
    statusListener match {
      case Some(b) => b !? StatusListener.GetStatus(uuid) match {
        case StatusListener.GetStatusResponse(statuses) => {
          renderText(statuses.map { 
            case (key, statuses) => "\"" + key + "\": " + statuses.map { "\"" + _ + "\"" }.mkString("[", ",", "]")
          }.mkString("{", ",", "}"))
        }
        case _ => renderText("{ error:\"Unknown uuid: " + uuid + "\"}");
      }
      case _ => renderText("{ error:\"Not started correctly\"}");
    }
  }

}