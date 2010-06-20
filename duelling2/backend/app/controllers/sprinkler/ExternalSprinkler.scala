package se.netlight.fn.imagesprinkler

import scala.actors.Actor
import scala.actors.Actor._

import org.mortbay.jetty._
import org.mortbay.jetty.servlet.{Context, ServletHolder, DefaultServlet}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

class HelloServlet(val name:String) extends DefaultServlet {
  override def doGet(request:HttpServletRequest, response:HttpServletResponse) {
    response.getWriter().println("hello " + name)
  }
}

class ExternalJsonSprinkler(backend:Backend, url:String) extends Actor {
  backend.register(this);

  // Startup listening server
  val server = new Server(8080);
  val root = new Context(server,"/",Context.SESSIONS);
  root.addServlet(new ServletHolder(new HelloServlet("Ciao")), "/*");
  server.start();
  
  var senders = Map[String, Actor]()

  def act() {
    while (true) {
      receive {
        case Photo(title, description, image) =>
          println("External sprinkler received photo")
        case Shutdown => 
          println("Shuting down external sprinkler")
      }
    }
  }
}