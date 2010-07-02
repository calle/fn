package controllers

import play._
import play.mvc._
import play.cache.Cache
import play.jobs._
import java.util.concurrent.Future;
import scala.actors.TIMEOUT
import scala.actors.OutputChannel
import scala.collection.mutable.{ Map => MMap, Buffer => MBuffer }

import imagesprinkler._
import imagesprinkler.listener.Listener
import imagesprinkler.sprinkler.Sprinkler


object StatusController extends Controller {

  var backend:Option[Backend] = None

  // Use Cache to store received statuses and deliver to clients
  private val listeners = MMap[String, StatusListener]()

  def index() = render()

  def setup() = {
  	listeners.get(session.getId()) match {
  	  case Some(listener) if listener.running => 
  		// Already running listener for this client, do nothing
  	  case _ =>  
        // Register new listener
        Logger.info("Registering new listener for client %s", session.getId())
        backend.map { b => 
          val listener = new StatusListener(b, session.getId())
          listeners += (session.getId() -> listener)
          Logger.debug("Starting listener for client %s", session.getId())
          listener.start
        }
    }
    
	// Fetch sprinklers
    renderText(backend.map { b => 
      b !? Backend.GetSprinklers match {
        case Backend.GetSprinklersResponse(sprinklers) => 
          sprinklers.map { s => format(s, true) }.mkString("[", ",", "]")
      }
    }.getOrElse("[]"))
  }

  def getStatuses() {
    listeners.get(session.getId()) match {
      case Some(listener) if listener.running => 
      	if (request.isNew) {
    	  Logger.info("Fetching statuses for listener " + session.getId());
    	  request.args.put("GetStatusesTask", new GetStatusesJob(listener).now())
      	  waitFor(request.args.get("GetStatusesTask").asInstanceOf[Future[Seq[Any]]])
      	}
      	Logger.debug("Returning statuses for listener " + session.getId())
      	renderText(request.args.get("GetStatusesTask") match {
      	  case task : Future[Seq[Any]] => task.get().map { format(_) }.mkString("[", ",", "]")
      	  case _ => "[]"
      	})

      case _ =>
      	// Redirect to setup 
		setup
    }
  }

  private def format(response:Any) : String = {
    response match {
      case Backend.RegisterSprinkler(s)   => format(s, true)
	  case Backend.UnregisterSprinkler(s) => format(s, false)
      case Started(s, i)                  => format(s, i, "\"started\": true")
      case InProgress(s, i, message)      => format(s, i, "\"status\": \"" + message + "\"")
      case Error(s, i, message)           => format(s, i, "\"error\": \"" + message + "\"")
      case Complete(s, i)                 => format(s, i, "\"done\": true")
    }
  }

  private def format(sprinkler:Sprinkler, registered:Boolean) = {
    "{ \"sprinkler\": { \"name\": \"" + sprinkler.name + "\", \"registered\": " + registered + " } }"
  }

  private def format(sprinkler:Sprinkler, instance:PhotoInstance, extra:String) = {
    "{ \"sprinkler\": { \"name\": \"" + sprinkler.name + "\" }, \"photo\": { \"id\": \"" + instance.photo.id + "\", \"title\": \"" + instance.photo.title + "\", " + extra + " } }"
  }

  private case object GetStatuses
  private case class  GetStatusesResponse(responses:Seq[Any])

  private class GetStatusesJob(listener:Listener) extends Job[Seq[Any]] {
	override def doJobWithResult() : Seq[Any] = {
	  listener !? GetStatuses match {
	    case GetStatusesResponse(responses) => responses
	    case _ => List()
	  }
	}
  }

  private class StatusListener(backend:Backend, id:String) extends Listener {
    var running = true
 
    val key = "statuses:" + id
    var enqueuedGetStatuses:Option[OutputChannel[Any]] = None
 
    def act() {
      Cache.set(key, MBuffer[Response](), "1min")
      Logger.debug("Registering StatusListener(%s)", id)
      backend !? Backend.RegisterListener(this)
      Logger.debug("Registering done for StatusListener(%s)", id)

      try {
      while (running) {
        receiveWithin(10000) {

          case status @ Backend.RegisterSprinkler(sprinkler) =>
            Logger.info("Received registration of sprinkler %s for StatusListener(%s)", sprinkler.name, id)
            appendStatus(status)

          case status @ Backend.UnregisterSprinkler(sprinkler) =>
            Logger.info("Received unregistration of sprinkler %s for StatusListener(%s)", sprinkler.name, id)
            appendStatus(status)

          case status : Response => 
            Logger.info("Received status %s for StatusListener(%s)", status, id)
            appendStatus(status)

          case GetStatuses => 
            Logger.info("Received GetStatuses for StatusListener(%s)", id)
            Cache.get[MBuffer[Any]](key) match {
              case Some(statuses) =>
              	if (statuses.size > 0) { 
                  Logger.debug("Found statuses: %s", statuses.mkString(", "))
	              reply(GetStatusesResponse(List(statuses : _*)))
                  statuses.clear
                } else {
                  // Enqueue request
                  Logger.debug("Enqueue GetStatuses request")
                  enqueuedGetStatuses = Some(sender);
                }
              case None => 
                Logger.debug("No statuses found")
                // Cache has expired, shutdown
                running = false
                reply(GetStatusesResponse(List()))
            }

		  case TIMEOUT => 
			enqueuedGetStatuses map { sender =>
	          	Logger.info("Response to previous GetStatuses request with empty list after timeout")
            	sender ! GetStatusesResponse(List())
            	enqueuedGetStatuses = None
          	}

          case Shutdown =>
            Logger.info("Received Shutdown for StatusListener(%s)", id)
            running = false

          case message => 
            Logger.warn("Received unknown message " + message + " for StatusListener(%s)", id)
        }
      }
      } catch {
    	  case e => Logger.warn("Got exception", e)
      }

      Logger.info("Shutting down StatusListener(%s)", id)
      backend !? Backend.UnregisterListener(this)
      Cache.delete(key)
      Logger.info("Deleted cache and shut down StatusListener(%s)", id)
    }
    
    def appendStatus(status:Any) {
      Cache.get[MBuffer[Any]](key) match {
        case Some(statuses) => 
          Logger.info("Appending status to statuses")
          statuses.append(status)
          
          // Send to awaiting threads
          enqueuedGetStatuses map { sender =>
          	Logger.info("Response to previous GetStatuses request")
            sender ! GetStatusesResponse(List(statuses : _*))
            statuses.clear
            enqueuedGetStatuses = None
          }
          
        case None => 
          Logger.info("No statuses found when appending status")
          // Cache has expired, shutdown
          running = false
      }
      
    }
  }

}