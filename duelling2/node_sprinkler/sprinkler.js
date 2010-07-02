var kiwi = require('kiwi'),
    sys = require('sys'),
    express = kiwi.require('express'),
    http = require('express/http'),
    View = require('express/plugins/view').View

var ROOT = "http://localhost:3000"
    BACKEND_ROOT = "http://localhost:9000/sprinkler/ext",
    NAME = "node"

var photos  = [],
    clients = {};

// Configure express
configure(function() {
  use(Logger)
  use(MethodOverride)
  use(ContentLength)
  use(Cookie)
  use(Session, { lifetime: (15).minutes, reapInterval: (1).minute })
  use(Static)
  set('root', __dirname)
})

// Utility method for sending a photo to a client
var sendPhoto = function(request, photo) {
  request.contentType('json')
  request.respond(200, JSON.encode(photo))
}

// Handle new photos
post('/photo', function() {
  var photo = { 
    id: this.param('id'),
    title: this.param('title'),
    description: this.param('description'),
    data: this.param('imageBase64')
  }

  // Push photo to array
  photos.push(photo)
  
  // Notify clients
  sys.puts("Sending photo to " + Object.keys(clients).length + " clients.")
  Object.keys(clients).forEach(function(id) {
    sendPhoto(clients[id], photo)
  })
  clients = {}
  
  this.respond(200, "done")
});

// Render start page for clients
get('/', function() {
  this.render('index.html.haml', {
    locals: { title: 'Image sprinkler' } 
  })
});

// Ajax callback to fetch next photo
get('/photo/next', function() {
  var seen = this.session.seen_photos || 0,
      enqueue = photos.length == 0 || seen >= photos.length,
      id = this.session.id;

  if (enqueue) {
    sys.puts("Enqueue client " + this.session.id)
    if (clients[id]) {
      // Respond to old request
      clients[id].contentType('json')
      clients[id].respond(200, '{}')
    }
    clients[id] = this
  } else {
    sys.puts("Sending photo " + seen + " to client " + id)
    sendPhoto(this, photos[seen])
    this.session.seen_photos = seen + 1
  }

  var request = this;
  setTimeout(function() {
    if (request === clients[id]) {
      delete clients[id]
    }
    request.contentType('json')
    request.respond(200, '{}')
  }, 10000)
})

// Start express server
run()

// Listen for Ctrl-C and unregister on shutdown
process.addListener('SIGINT', function () {
  http.post(BACKEND_ROOT + "/unregister", { name: NAME }, function() {
    process.exit(0)
  })
});

// Register sprinkler
http.post(BACKEND_ROOT + "/register", { name: NAME, url: ROOT + '/photo' }, function() {
  sys.puts("Registration completed")
})
