print 'Content-Type: image/png'
print ''
from google.appengine.ext import db
#db.Blob(open("image.png", "rb").read())
#open("image.png", "rb").print()
response.out.write("hello")
