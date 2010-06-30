import cgi
import datetime
import logging

from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.api import images

logging.getLogger().setLevel(logging.DEBUG)

class AnImage(db.Model):
    image = db.BlobProperty()
    date = db.DateTimeProperty(auto_now_add=True)

class MainPage(webapp.RequestHandler):
    def get(self):
        self.response.out.write('<html><body>')
        self.response.out.write("""
              <form action="/save" enctype="multipart/form-data" method="post">
                <div><label>Image:</label></div>
                <div><input type="file" name="img"/></div>
                <div><input type="submit" value="Submit image"></div>
              </form>
            </body>
          </html>""")

class Image (webapp.RequestHandler):
    def get(self):
        try:
            animage = db.get(self.request.get("img_id"))
            if animage.image:
                self.response.headers['Content-Type'] = "image/png"
                self.response.out.write(animage.image)
            else:
                self.response.out.write("No image")
        except:
            self.response.out.write("No image")

class SaveImage(webapp.RequestHandler):
    def post(self):
        animage = AnImage()
        foo = images.resize(self.request.get("img"),100,100)
        animage.image = db.Blob(foo)
        animage.put()
        self.redirect('/img?img_id=%s' % animage.key())


application = webapp.WSGIApplication([
    ('/', MainPage),
    ('/img', Image),
    ('/save', SaveImage)
], debug=True)


def main():
    run_wsgi_app(application)


if __name__ == '__main__':
    main()
