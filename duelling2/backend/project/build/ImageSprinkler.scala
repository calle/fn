import sbt._

class ImageSprinklerProject(info: ProjectInfo) extends DefaultProject(info) {

  val jetty6 = "org.mortbay.jetty" % "jetty" % "6.1.22"

  override def mainClass = Some("se.netlight.fn.imagesprinkler.Main")

}