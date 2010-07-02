var kiwi = require('kiwi'),
    sys = require('sys'),
    express = kiwi.require('express'),
    http = require('express/http'),
    View = require('express/plugins/view').View

var ROOT = "http://localhost:3000"
    BACKEND_ROOT = "http://localhost:9000/sprinkler/ext",
    NAME = "node"

var photos  = [],
	users   = {},
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

// Render start page for clients
get('/', function() {
  if (users[this.session.id]) {
    this.render('photo.html.haml', {
	  locals: { title: 'Image sprinkler', username: users[this.session.id].username } 
	})
  } else {
	  this.render('index.html.haml', {
		  locals: { title: 'Image sprinkler' } 
	  })
  }
});

post('/signup', function() {
	var user = {
      id: this.session.id,
	  username: this.param('name'),
	  photos: []
	};
	
	users[this.session.id] = user;

	// Register sprinkler
	var request = this;
	http.post(BACKEND_ROOT + "/register", { name: NAME + "." + user.username, url: ROOT + '/photo/' + user.id }, function() {
		sys.puts("Registered " + user.username);
		request.redirect("/")
	})
})

get('/photo/first', function() {
  var user = users[this.session.id];
  sendPhoto(this, user && user.last_photo)
});

// Ajax callback to fetch next photo
get('/photo/next', function() {
  var user = users[this.session.id];

  if (!user) {
	sys.puts("Cannot find user " + this.session.id)
	response(200, "{}");
  }

  var photo = user.photos.shift();
  
  if (photo) {
    sys.puts("Sending photo " + seen + " to " + user.username)
    sendPhoto(this, photo)
  } else {
    sys.puts("No photo found for " + user.username + ": " + user.photos.join(","))
    if (user.request) {
      // Respond to old request
      sys.puts("Terminate existing request for " + user.username)
      sendPhoto(user.request, {})
    }
    sys.puts("Save request for " + user.username)
    user.request = this
  }

  var request = this;
  request.timeout = setTimeout(function() {
	sys.puts("Timeout received for " + user.username)
    if (request === user.request) {
   	  sys.puts("Request timing out is current request for " + user.username)
      // Remove callback
      delete user.request
    }
	sys.puts("Terminating request for " + user.username)
    sendPhoto(request, {})
  }, 10000)
})

// Utility method for sending a photo to a client
var sendPhoto = function(request, photo) {
  if (request.timeout) { clearTimeout(request.timeout) }
  request.contentType('json')
  request.respond(200, JSON.encode(photo))
}

// Handle new photos
post('/photo/:user', function() {

  var user = users[this.param('user')];
  if (!user) {
	  // cannot find user information
    sys.puts("Cannot find user " + this.param('user'))
	return;
  }

  var photo = { 
    id: this.param('id'),
    title: this.param('title'),
    description: this.param('description'),
    data: this.param('imageBase64')
  }

  user.last_photo = photo;

  sys.puts("Received photo " + photo.id + " for user " + user.username)

  if (user.request) {
	// Relay to client directly
    sys.puts("Relay new photo to " + user.username + " directly")
	sendPhoto(user.request, photo)
	delete user.request
  } else {
    // Push photo to array
	sys.puts("Push photo to photos array of " + user.username)
	user.photos.push(photo)
  }

  this.respond(200, "done")
});


// Start express server
run()

// Listen for Ctrl-C and unregister on shutdown
process.addListener('SIGINT', function () {
  var userIds = Object.keys(users);
  var userIdCount = userIds.length;
  var completed = 0;
  userIds.forEach(function(id) {
	var user = users[id]
    if (user.request) {
	  // Relay to client directly
	  sendPhoto(user.request, {})
	  delete user.request
    }
    http.post(BACKEND_ROOT + "/unregister", { name: NAME + "." + user.username }, function() {
      completed += 1;
      if (completed >= userIds.length) { process.exit(0) }
    })
  })
})

