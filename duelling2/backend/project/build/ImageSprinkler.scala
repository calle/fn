import sbt._

class ImageSprinklerProject(info: ProjectInfo) extends DefaultProject(info) {
  override def mainClass = Some("se.netlight.fn.imagesprinkler.Main")
}