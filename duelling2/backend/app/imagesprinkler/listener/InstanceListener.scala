package imagesprinkler.listener

import imagesprinkler._
import imagesprinkler.sprinkler._

object InstanceListener {

  private class TheActor(instance:InstanceListener, backend:Backend, photo:Photo) extends Listener {
    def act() {
      backend !? Backend.RegisterListener(this)
      backend ! Send(photo)

      val uuid = photo.id
      var running = true
      while (running) {
        receive {
          case status @ Started(_, PhotoInstance(photo, _)) if photo.id == uuid => 
            instance.started(status)

          case status @ InProgress(_, PhotoInstance(photo, _), _) if photo.id == uuid =>
            instance.inProgress(status)

          case status @ Complete(_, PhotoInstance(photo, _)) if photo.id == uuid => 
            instance.complete(status)
            running = false

          case status @ Error(_, PhotoInstance(photo, _), _) if photo.id == uuid => 
            instance.error(status)
            running = false

          case Shutdown => 
            running = false
        }
      }

      backend ! Backend.UnregisterListener(this)
    }
  }
}

abstract class InstanceListener(backend:Backend) {

  def send(photo:Photo) {
    val actor = new InstanceListener.TheActor(this, backend, photo)
    actor.start
  }

  def started(status:Started) : Unit
  def inProgress(status:InProgress) : Unit
  def complete(status:Complete) : Unit
  def error(status:Error) : Unit
  
}
