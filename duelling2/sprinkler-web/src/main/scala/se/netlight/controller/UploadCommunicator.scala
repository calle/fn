package se.netlight.controller

import scala.collection.mutable.{HashMap, HashSet}
import scala.actors.Actor
import scala.actors.Actor._
import org.apache.commons.codec.binary.Base64

import net.liftweb.util.Helpers._

case class UploadFile(file: File)
case class AddListener(listener: Actor)
case class UpdateStatus(id: String, status: String)
case class UpdateActorStatus(status: String)
case class GetId

case class File(id: String, title: String, desc: String, data: Array[Byte])


object UploadCommunicator extends Actor {
	val actors = new HashSet[Actor]
	var idCounter = 0
	val b64Encoder = new Base64()
	
	def notifyListeners(status: String) = {
		actors.foreach(_ ! UpdateActorStatus(status))
	}
	
	def performUpload(file: File) = {
		val encodedFile = b64Encoder.encode(file.data)
		System.out.println("Encoded file! Name is " + file.title)
		System.out.println("Id is " + file.id)
		notifyListeners("Uploading " + file.title)
	}
	
	def act = {
	  loop {
	    react {
				case AddListener(listener) =>
				  System.out.println("Adding listener..")
				  actors += listener
	      case UploadFile(file: File) =>
	        performUpload(file)
	      case UpdateStatus(id: String, status: String) => 
					notifyListeners(status)
				case GetId => {
					idCounter = idCounter + 1
					reply(String.valueOf(idCounter))
				}
	    }
	  }
	}

	start  // This starts our singleton Actor
}