package imagesprinkler

import java.io.File

abstract class Image {
  def asBase64:String
  def asFile:File
}
class ImageFile(image:File) extends Image {
  def asBase64 = play.libs.Images.toBase64(image)
  def asFile = image
}

case class Photo(id:String, title:String, description:String, image:Image)
