package se.netlight.snippet

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import scala.xml.{NodeSeq, Text, Group}

class UploadHandler {
	private object uploadedFile extends RequestVar[Box[FileParamHolder]](Empty)
	
	def upload(xhtml: Group): NodeSeq = {
		if (get_?) bind("ul", chooseTemplate("choose", "get", xhtml), 
		         "file_upload" -> fileUpload(ul => uploadedFile(Full(ul))))
		else bind("ul", chooseTemplate("choose", "post", xhtml),
						  "file_name" -> uploadedFile.is.map(v => Text(v.fileName))
		);
						
	}
}  
