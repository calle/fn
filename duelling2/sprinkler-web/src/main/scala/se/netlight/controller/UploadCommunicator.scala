package se.netlight.controller

import scala.collection.mutable.{HashMap, HashSet}
import scala.actors.Actor
import scala.actors.Actor._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._
import se.netlight.comet.{UploadActor}

import net.liftweb.util.Helpers._

case class UploadFile(file: File)
case class AddListener(listener: UploadActor)
case class UpdateStatus(id: String, status: String)
case class UpdateActorStatus(status: String)
case class GetId

case class File(id: String, title: String, desc: String, mimeType: String, data: Array[Byte])

object UploadCommunicator extends Actor {
	val actors = new HashSet[UploadActor]
	var idCounter = 0
	val b64Encoder = new Base64()
	val httpClient = new HttpClient()
    val callbackUrl = "http://mini.calle.cc:9002/callback"
	
	def notifyListeners(status: String) = {
		actors.foreach(_ ! UpdateActorStatus(status))
	}
	
	def performUpload(file: File) = {
		val encodedFile = b64Encoder.encodeToString(file.data)
		System.out.println("Encoded file! Name is " + file.title)
		System.out.println("Id is " + file.id)
		System.out.println("mimetype: " + file.mimeType)
		System.out.println("Description: " + file.desc)
		notifyListeners("Uploading " + file.title)
		val method = new PostMethod("http://backend.fnnl.se/imageWithCallback")
		method.addParameter("title", file.title)
		method.addParameter("description", file.desc)
		method.addParameter("image", "data:" + file.mimeType +";base64," + encodedFile)
		method.addParameter("callback", callbackUrl)
		
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
	        System.out.println("Got status: " + status)
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
