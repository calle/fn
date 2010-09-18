var express = require('express'),
    connect = require('connect'),
    Battlefield = require('./lib/battlefield');

var app = module.exports = express.createServer();
var battlefield = new Battlefield('localhost', 3001);

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
  battlefield.login(id, req.param('name'), {
    login: function(err, response) {
      res.send(err ? 503 : response);
    },
    update: function(message) {
      if (!req.session) {
        battlefield.logout(id, function() {});
      } else {
        req.session.statuses.push("updated:" + message);
        
        if (req.session.statusTimeout) {
          cancelTimeout(req.session.statusTimeout);
        }
      }
    },
    error: function() {
      if (req.session) {
        req.session.statuses.push("error");
      }
    }
    end: function() {
      if (req.session) {
        req.session.statuses.push("terminated");
      }
    },
  });
});

app.post('/status', function(req, res) {
  if (req.session.statuses && req.session.statuses.length > 0) {
    res.send(req.sessions.statuses);
    req.session.statuses = [];
  } else {
    req.session.statusTimeout = setTimeout(function() {
      res.send([])
    }, 1000);
  }
});

app.post('/fire', function(req, res) {
  battlefield.shoot(req.session.id, {
      x: req.param('x'), 
      y: req.param('y')
    }, function(err, response) {
      res.send(err ? 503 : response);
    });
});

app.post('/move', function(req, res) {
  battlefield.move(req.session.id, req.param('direction'), 
    function(err, response) {
      res.send(err ? 503 : response);
    });
});

app.post('/logout', function(req, res) {
  battlefield.logout(req.session.id, function() {
    res.send(200);
  })
});

app.post('/taunt', function(req, res) {
  battlefield.taunt(req.session.id, req.param('name'), req.param('insult'));
  res.send(200);
});

// Only listen on $ node app.js
if (!module.parent) app.listen(3000);
