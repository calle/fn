package imagesprinkler

import java.io.File

abstract class Image {
  def asBase64:String
}
class ImageFile(image:File) extends Image {
  private val data = play.libs.Images.toBase64(image)
  def asBase64 = data
}
class ImageBase64(data:String) extends Image {
  def asBase64 = data
}

case class Photo(id:String, title:String, description:String, image:Image)
