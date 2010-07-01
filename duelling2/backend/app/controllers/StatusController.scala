package controllers

import play._
import play.mvc._
import play.cache.Cache
import scala.collection.mutable.{ Map => MMap, Buffer => MBuffer }

import imagesprinkler._
import imagesprinkler.listener.Listener
import imagesprinkler.sprinkler.Sprinkler


object StatusController extends Controller {

  var backend:Option[Backend] = None

  // Use Cache to store received statuses and deliver to clients
  val listeners = MMap[String, Listener]()

  def index() = render()

  def getStatuses() {
    listeners.get(session.getId()) match {
      case Some(listener) => 
        listener !? GetStatuses match {
          case GetStatusesResponse(responses) => {
            renderText(responses.map { response =>
              response match {
                case Started(s, i)             => format(s, i, "started: true")
                case InProgress(s, i, message) => format(s, i, "status: \"" + message + "\"")
                case Error(s, i, message)      => format(s, i, "error: \"" + message + "\"")
                case Complete(s, i)            => format(s, i, "done: true")
              }
            }.mkString("[", ",", "]"))
          }
        }
      case None => 
        // Register new listener
        Logger.info("Registering new listener for client %s", session.getId())
        backend.map { b =>
          val listener = new StatusListener(b, session.getId())
          listeners += (session.getId() -> listener)
          Logger.debug("Starting listener for client %s", session.getId())
          listener.start
        }
        renderText("{}")
    }
  }

  private def format(sprinkler:Sprinkler, instance:PhotoInstance, extra:String) = {
    "{ sprinkler: \"" + sprinkler.name + "\", photo: \"" + instance.photo.id + "\", " + extra + " }"
  }

  private case object GetStatuses
  private case class  GetStatusesResponse(responses:Seq[Response])

  private class StatusListener(backend:Backend, id:String) extends Listener {
    var running = true
 
    val key = "statuses:" + id
 
    def act() {
      Cache.set(key, MBuffer[Response](), "1min")
      Logger.debug("Registering StatusListener(%s)", id)
      backend !? Backend.RegisterListener(this)
      Logger.debug("Registering done for StatusListener(%s)", id)

      while (running) {
        receive {
          case status : Response => 
            Logger.info("Received status %s for StatusListener(%s)", status, id)
            appendStatus(status)

          case GetStatuses => 
            Logger.info("Received GetStatuses for StatusListener(%s)", id)
            Cache.get[MBuffer[Response]](key) match {
              case Some(statuses) => 
                Logger.debug("Found statuses: %s", statuses.mkString(", "))
                reply(GetStatusesResponse(List(statuses : _*)))
                statuses.clear
              case None => 
                Logger.debug("No statuses found")
                // Cache has expired, shutdown
                running = false
                reply(GetStatusesResponse(List()))
            }

          case Shutdown =>
            Logger.info("Received Shutdown for StatusListener(%s)", id)
            running = false
            
          case message => 
            Logger.warn("Received unknown message " + message + " for StatusListener(%s)", id)
        }
      }

      Logger.info("Shutting down StatusListener(%s)", id)
      backend !? Backend.UnregisterListener(this)
      Cache.delete(key)
      Logger.info("Deleted cache and shut down StatusListener(%s)", id)
    }
    
    def appendStatus(status:Response) {
      Cache.get[MBuffer[Response]](key) match {
        case Some(statuses) => 
          Logger.info("Appending status to statuses")
          statuses.append(status)
        case None => 
          Logger.info("No statuses found when appending status")
          // Cache has expired, shutdown
          running = false
      }
      
    }
  }

}