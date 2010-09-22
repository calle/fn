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

app.post('/login', function(req, res) {
  
  req.session.statuses = [];
  var id = req.session.id;

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
      res.send(err ? 503 : response);
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
      if (!alive()) {
        client.logout(function() {});
      } else {
        req.session.statuses.push("updated:" + type + ':' + JSON.stringify(arguments));
        if (req.session.statusTimeout) {
          cancelTimeout(req.session.statusTimeout);
        }
      }
    });
  });
});

app.post('/status', function(req, res) {
  if (req.session && req.session.statuses && req.session.statuses.length > 0) {
    res.send(req.sessions.statuses);
    req.session.statuses = [];
  } else {
    req.session.statusTimeout = setTimeout(function() {
      res.send([])
    }, 1000);
  }
});

app.post('/fire', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.shoot({
        x: req.param('x'),
        y: req.param('y')
      }, function(err, response) {
        res.send(err ? 503 : response);
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
        res.send(err ? 503 : response);
      });
  } else {
    res.send(403);
  }
});

app.post('/logout', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.logout(function(err, response) {
        res.send(err ? 503 : response);
      });
  } else {
    res.send(403);
  }
});

app.post('/taunt', function(req, res) {
  if (req.session && req.session.client) {
    req.session.client.taunt(req.param('name'), req.param('insult'), function(err, response) {
      res.send(err ? 503 : response);
    });
  } else {
    res.send(403);
  }
});

// Only listen on $ node app.js
if (!module.parent) app.listen(3000);
