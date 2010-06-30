package se.netlight.comet

import scala.collection.mutable.HashMap
import scala.xml.{NodeSeq, Text}

import scala.actors.Actor._  
import net.liftweb.http._
import net.liftweb.util._

import se.netlight.controller._

class UploadActor extends CometActor {
	override val defaultPrefix = Full("st")
	var currStatus = ""
	private object uploadedFile extends RequestVar[Box[FileParamHolder]](Empty)
	
	def render = {
		bind("status" -> <div>{currStatus}</div>)
	}
	
	override def lowPriority : PartialFunction[Any, Unit] = {
	  case UpdateActorStatus(status) => currStatus = status; reRender(false)
	}
	
	override def localSetup {
		UploadCommunicator ! AddListener(this)
	}
}