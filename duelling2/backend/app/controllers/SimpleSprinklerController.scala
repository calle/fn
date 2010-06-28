package controllers

import play._
import play.mvc._
import play.jobs._
import java.util.concurrent.Future

import imagesprinkler.Photo
import imagesprinkler.sprinkler.SimpleSprinkler

object SimpleSprinklerController extends Controller {
	
  var sprinkler:Option[SimpleSprinkler] = None

  def index = render()

  def photo = {
	sprinkler match {
	  case Some(sprinkler) => {
	    sprinkler !? SimpleSprinkler.GetPhoto match {
	      case SimpleSprinkler.GetPhotoResult(Some(photo)) => renderPhoto(photo)
	      case _ => renderText("{}")
	    }
	  }
	  case _ => renderText("{}")
	}
  }

  def nextPhoto = {
    if(request.isNew) {
        val task = new GetNextPhotoJob().now()
        request.args.put("task", task)
        waitFor(task)
    }
    request.args.get("task") match {
      case future:Future[Option[Photo]] => future.get() match {
    	case Some(photo) => renderPhoto(photo)
    	case _ => renderText("{}")
      }
    }
  }

  private def renderPhoto(photo:Photo) = 
	renderJSON(JsonPhoto(photo.id, photo.image.asBase64))
  
  private case class JsonPhoto(id:String, data:String) 

  private class GetNextPhotoJob extends Job[Option[Photo]] {
	override def doJobWithResult() : Option[Photo] = {
	  SimpleSprinklerController.sprinkler match {
	    case Some(sprinkler) => {
	 	  sprinkler !? SimpleSprinkler.GetNextPhoto match {
	        case SimpleSprinkler.GetPhotoResult(Some(photo)) => Some(photo)
	        case _ => None
	      }
	 	}
	 	case _ => None
	  }
	}
  }

}