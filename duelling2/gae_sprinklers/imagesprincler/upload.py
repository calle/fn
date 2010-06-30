import cgi
import datetime
import logging

from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.api import images

from tumblr import Api, TumblrAuthError, TumblrRequestError, TumblrError

BLOG = 'imagesprinkler.tumblr.com'
USER = 'lennartsson+imagesprinkler@gmail.com'
PASSWORD = 'func1234net'

logging.getLogger().setLevel(logging.DEBUG)

class AnImage(db.Model):
    image = db.BlobProperty()
    date = db.DateTimeProperty(auto_now_add=True)

class MainPage(webapp.RequestHandler):
    def get(self):
        self.response.out.write('<html><body>')
        self.response.out.write("""
              <form action="/save" enctype="multipart/form-data" method="post">
                <div><label>Title:</label></div>
                <div><input type="text" name="title"/></div>
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
        title = self.request.get("title")
        if(title == None):
            title = "Image Sprinkler"
        foo = images.resize(self.request.get("img"),640,480)
        animage.image = db.Blob(foo)
        animage.put()
        url = 'http://imagesprincler.appspot.com/img?img_id=%s' % animage.key()
#        url = 'http://localhost:8080/img?img_id=%s' % animage.key()
        self.post_to_tumblr(title, url)
        #self.redirect('/img?img_id=%s' % animage.key())
        self.response.out.write(url)

    def post_to_tumblr(self, title, url):
        api = Api(BLOG, USER, PASSWORD)
        try:
            html = '<img src="%s" alt="%s" />' % (url, title)
            api.write_regular(title, html)
        except TumblrError, e:
            logging.error(e)


application = webapp.WSGIApplication([
    ('/', MainPage),
    ('/img', Image),
    ('/save', SaveImage)
], debug=True)


def main():
    run_wsgi_app(application)


if __name__ == '__main__':
    main()
