var express = require('express'),
    connect = require('connect'),
    Client = require('./lib/client/client'),
    ServerProxySocket = require('./lib/server/server_proxy_socket');

var app = module.exports = express.createServer();

// Configuration

app.configure(function(){
  app.set('views', __dirname + '/views');
  app.use(connect.cookieDecoder());
  app.use(connect.session());
  app.use(connect.bodyDecoder());
  app.use(connect.methodOverride());
  app.use(connect.compiler({ src: __dirname + '/public', enable: ['less'] }));
  app.use(app.router);
  app.use(connect.staticProvider(__dirname + '/public'));
});

app.configure('development', function(){
  app.use(connect.errorHandler({ dumpExceptions: true, showStack: true }));
});

app.configure('production', function(){
 app.use(connect.errorHandler());
});

// Routes

app.get('/', function(req, res){
  res.render('index.jade', {
    locals: {
      title: 'Battlefield'
    }
  });
});

app.post('/state', function(req, res) {
  if (req.session && req.session.client && req.session.client.loggedIn) {
    res.send({
      name:      req.session.client.name,
      board:     { width:req.session.client.board.width, height:req.session.client.board.height },
      position:  req.session.client.position,
      direction: req.session.client.direction,
      size:      req.session.client.size,
      clients:   req.session.client.clients
    });
  } else {
    res.send(403);
  }
});

app.post('/login', function(req, res) {

  req.session.statuses = [];

  // Create server
  var server = new ServerProxySocket('localhost', 9000);
  var client = req.session.client = new Client(server);

  var alive = function() {
    return req.session && req.session.statuses &&
      req.session.client === client;
  };

  // Setup server events
  server.on('connected', function () {
    client.login(req.param('name'), function(err, response) {
      res.send(err || response);
    });
  });
  server.on('error', function(err) {
    if (alive()) req.session.statuses.push("error:"+err);
  });
  server.on('closed', function() {
    if (alive()) req.session.statuses.push("terminated");
  });
  
  [ 'userLogin', 'userLogout', 'userMoved', 'userKilled', 'killed', 'taunted'
  ].forEach(function(type) {
    client.on(type, function() {
      console.log('app.js[%s]: Received update to %s: %j', req.sessionID, type, arguments)
      if (!alive()) {
        client.logout(function() {});
      } else {
        req.session.statuses.push("updated:" + type + ':' + JSON.stringify(arguments));
        if (statusTimeouts[req.sessionID]) {
          statusTimeouts[req.sessionID]();
        }
      }
    });
  });
});

var statusTimeouts = {};

app.post('/status', function(req, res) {
  console.log('app.js[%s]: Fetching statuses: %d', req.sessionID, req.session.statuses ? req.session.statuses.length : -1);
  if (req.session && req.session.statuses && req.session.statuses.length > 0) {
    res.send(req.session.statuses);
    // Clear 
    while (req.session.statuses.length > 0) req.session.statuses.shift();
  } else {
    statusTimeouts[req.sessionID] = function() { 
      res.send([]); 
      delete statusTimeouts[req.sessionID];
    }
    setTimeout(statusTimeouts[req.sessionID], 10000);
  }
});

app.post('/fire', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.shoot({
        x: req.param('x'),
        y: req.param('y')
      }, function(err, response) {
        res.send(err ? 500 : response);
      }
    );
  } else {
    res.send(403);
  }
});

app.post('/move', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.move(req.param('direction'),
      function(err, response) {
        res.send(err ? 500 : response);
      });
  } else {
    res.send(403);
  }
});

app.post('/logout', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.logout(function(err, response) {
        res.send('');
      });
  } else {
    res.send(403);
  }
});

app.post('/taunt', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.taunt(req.param('name'), req.param('insult'), function(err, response) {
      res.send(err ? 500 : response);
    });
  } else {
    res.send(403);
  }
});

// Only listen on $ node app.js
if (!module.parent) app.listen(3000);
