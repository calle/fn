package se.netlight.snippet

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import scala.xml.{NodeSeq, Text, Group}
import se.netlight.controller._
import net.liftweb.common._

class UploadHandler {
	private object uploadedFile extends RequestVar[Box[FileParamHolder]](Empty)
	
	def upload(xhtml: Group): NodeSeq = {
		if (get_?) bind("ul", chooseTemplate("choose", "get", xhtml), 
		         "file_upload" -> fileUpload(ul => uploadedFile(Full(ul))))
		else {
			uploadedFile.is match {
				case Full(file) => {
					val name: String = file.fileName
					val data: Array[Byte] = file.file

					UploadCommunicator !? GetId match {
						case id:String => {
							UploadCommunicator ! UploadFile(File(id, name, "Description goes here", data))
							bind("ul", chooseTemplate("choose", "post", xhtml),
										  "file_name" -> uploadedFile.is.map(v => Text(v.fileName))
							);
						}
					}
				}
				case Empty =>
					bind("ul", chooseTemplate("choose", "error", xhtml))
			}			
		}
						
	}
}  
