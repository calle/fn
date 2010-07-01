import sbt._

class BackendProject(info: ProjectInfo) extends DefaultProject(info) {

  val play_root = Path.fromFile(new java.io.File("/Users/calle/code/external/play-1.1/"))

  override def mainScalaSourcePath = "app"
  // override val mainSources = ("app" / "imagesprinkler" ** "*.scala") +++ ("app" / "models" ** "*.scala")
  // override def outputDirectoryName = "tmp"

  def playDirectories = (play_root / "framework") +++ (play_root / "framework" / "lib")
  override def unmanagedClasspath = descendents(playDirectories, "*.jar") +++ 
    (play_root / "modules" / "play-scala" / "lib" / "play-scala.jar")

}
