package se.netlight.controller

import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.common._

object CallbackHandler extends RestHelper {
	
	def notifyHandlers() = {
		for {
			id <- S.param("id") ?~ "ID parameter missing" ~> 400
			status <- S.param("status") ?~ "Status parameter missing" ~> 400
		} yield {
			UploadCommunicator ! UpdateStatus(id, status)
			OkResponse()
		}
	}
	
	/*def dispatch: LiftRules.DispatchPF = {
	  case RequestMatcher(r, ParsePath("callback" :: _, "", PostRequest), _, _) => notifyHandlers
	}*/
	
	serve {
	    case Post("callback" :: _, _) => notifyHandlers()
	}
}