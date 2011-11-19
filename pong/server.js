var express = require('express')
  , fs      = require('fs')
  , path    = require('path');

// Static configuration
var ROOT_DIR   = __dirname
  , STATIC_DIR = path.join(ROOT_DIR, 'public');

/**
 * Setup express
 */
var app = express.createServer();

app.configure(function(){

  // Plugins
  app.use(express.cookieParser());
  app.use(express.session({ secret: 'fnnl-pong' }));
  app.use(express.methodOverride());
  app.use(express.bodyParser());
  app.use(app.router);
  app.use(express['static'](STATIC_DIR));

});

app.configure('development', function() {
  app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
});

app.configure('production', function() {
  app.use(express.errorHandler());
});

/**
 * Start server
 */
require('node-server').server(function(config) {
  console.log('Starting server', config);

  /**
   * Socket.IO
   */

  var io = require('socket.io').listen(app);

  /**
   * browserify
   */
  var bundle = require('browserify')(path.join(ROOT_DIR, 'lib/game.js'));
  app.use(bundle);

  /**
   * Server code
   */

  global._   = require('./public/javascripts/vendor/underscore');
  var Game   = require('./lib/game')
    , Player = require('./lib/player')
    , Ball   = require('./lib/ball');

  var game_attributes = {
    width  : 100
  , height : 100
  };
  var player_attributes = {
    x      : game_attributes.width - 1
  , y      : 45
  , width  : 1
  , height : 10
  , speed  : 20
  };
  var ball_attributes = {
    x      : 10
  , y      : 20
  , width  : 1
  , height : 1
  , speed  : 10
  };

  // Create game and initial enemy
  var game = new Game(game_attributes);
  game.addPlayer('enemy', {
    x      : 0
  , y      : 0
  , width  : player_attributes.width
  , height : game_attributes.height
  , speed  : player_attributes.speed
  });

  var addBall = function() {
    var ball = game.addBall(ball_attributes);
    ball.once('dead', function() {
      setTimeout(function() {
        game.removeBall(ball.id);
        addBall();
      }, 3000);
    });
  };

  // Trigger update of game 30 times / second
  setInterval(function() {
    game.tick();
  }, 1000 / 30);

  // Handle client connection
  io.sockets.on('connection', function(socket) {

    // Connected clients can send 'game state' and 'join game'

    socket.on('game state', function(cb) {
      cb(null, game.getState());
    });

    socket.on('join game', function(name, cb) {

      socket.has('player', function(err, exists) {
        if (err) return cb(new Error('Internal error: ' + err.message));

        if (exists) {
          return cb(new Error('already logged in'));
        }

        var players    = game.getPlayers()
          , nameExists = players.some(function(player) {
              return player.get('name') === name;
            });

        // TODO: Disallow duplicate names
        nameExists = false;

        if (nameExists) {
          return cb(new Error('name exists'));
        }

        // Add player
        var player = game.addPlayer(name, player_attributes);

        // Add ball if this is first player and no balls exists
        if (game.getPlayers().length === 2 && game.getBalls().length === 0) {
          addBall();
        }

        // Save player to socket
        socket.set('player', player, function (err) {
          if (err) return cb(new Error('Internal error: ' + err.message));

          // Listen for messages from this client
          socket.on('move', function(direction) {
            socket.get('player', function(err, player) {
              if (player) {
                // Start moving
                player.move(direction);
                // Broadcast player move
                io.sockets.emit('move player', player.id, player.getPosition(), direction);
              }
            });
          });

          socket.on('stop', function(direction) {
            socket.get('player', function(err, player) {
              if (player) {
                // Stop moving
                player.stop();
                // Broadcast player stop and position
                io.sockets.emit('stop player', player.id, player.getPosition());
              }
            });
          });
        
          // Broadcast ball states every third second
          socket.set('ball states interval', setInterval(function() {
            io.sockets.emit('object states', game.getObjectStates(Ball.TYPE));
          }, 3000));

          // Invoke callback with player
          cb(null, player.id);
        });

      });

    });

    socket.on('disconnect', function () {
      // Stop broadcasting ball states
      socket.get('ball states interval', function(err, interval) {
        if (interval) clearInterval(interval);
      });
      // Remove player from game
      socket.get('player', function(err, player) {
        if (player) {
          game.removePlayer(player.id);
          socket.del('player');
        }
      });
    });

  });

  // Forward game events to clients over socket.io

  game.on('add object', function(object) {
    io.sockets.emit('add object', object.getState());
  });

  game.on('remove object', function(object) {
    io.sockets.emit('remove object', object.id);
  });

  game.on('ball dead', function(ball, edge, score) {
    io.sockets.emit('ball dead', ball.id, edge, score);
  });


  /**
   * Start listening
   */
  app.listen(config['--port'] || 3000);
  console.log("Server listening on port %d", app.address().port);
});