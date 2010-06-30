package se.netlight.controller

import scala.collection.mutable.{HashMap, HashSet}
import scala.actors.Actor
import scala.actors.Actor._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._

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
	val httpClient = new HttpClient()
	
	def notifyListeners(status: String) = {
		actors.foreach(_ ! UpdateActorStatus(status))
	}
	
	def performUpload(file: File) = {
		val encodedFile = b64Encoder.encodeToString(file.data)
		System.out.println("Encoded file! Name is " + file.title)
		System.out.println("Id is " + file.id)
		System.out.println("Data: " + encodedFile)
		notifyListeners("Uploading " + file.title)
		val method = new PostMethod("http://mini.calle.cc:9000/imageWithCallback")
		method.addParameter("title", file.title)
		method.addParameter("description", file.desc)
		method.addParameter("image", "data:image/png;base64," + encodedFile)
		method.addParameter("callback", "callback")
		
		try {
			val statusCode = httpClient.executeMethod(method)
			
			System.out.println("Status from POST was " + statusCode)
		} catch {
			case e => System.out.println("Error on post! " + e.getMessage)
		}
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