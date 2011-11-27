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

  io.enable('browser client minification');  // send minified client
  io.enable('browser client etag');          // apply etag caching logic based on version number
  io.enable('browser client gzip');          // gzip the file
  io.set('log level', 1);                    // reduce logging
  io.set('transports', [                     // enable all transports (optional if you want flashsocket)
      'websocket'
    , 'flashsocket'
    , 'htmlfile'
    , 'xhr-polling'
    , 'jsonp-polling'
  ]);

  /**
   * browserify
   */
  var bundle = require('browserify')(path.join(ROOT_DIR, 'lib/game.js'));
  app.use(bundle);

  /**
   * Server code
   */

  // Need to pollute global namespace with underscore and Box2d
  var vm      = require('vm')
    , context = vm.createContext({});
  [ './public/javascripts/vendor/underscore'
  // , './public/javascripts/vendor/Box2dWeb-2.1.a.3.js'
  ].forEach(function (filename) {
    vm.runInContext(fs.readFileSync(require.resolve(filename), 'utf-8'), context, filename);
  });
  global._     = context._;
  global.Box2D = context.Box2D;

  var Game = require('./lib/game')
    , messages = [];

  var game_attributes = {
    size      : { width: 10, height: 6 }
  , time_step : 1/30
  };
  var ball_attributes = {
    position : { x: 2, y: 5 }
  , size     : { width: 0.1, height: 0.1 }
  , speed    : 1
  };
  var player_attributes = {
    position : { x: game_attributes.size.width - 0.1, y: 0 }
  , size     : { width: 0.3, height: 0.3 }
  , speed    : 2
  , movement : { friction: 0.9, force: 3 }
  };
  var enemy_attributes = {
    position : { x: 0, y: 0 }
  , size     : { width: player_attributes.size.width, height: 2 } // game_attributes.size.height }
  , bursts   : player_attributes.bursts
  };

  // Create game and initial enemy
  var game = new Game(game_attributes);
  // var enemy = game.addPlayer('enemy', enemy_attributes);

  var addBall = function() {
    var ball = game.addBall(ball_attributes);
    ball.once('dead', function() {
      game.removeBall(ball.id);
      setTimeout(addBall, 3000);
    });
  };

  // addBall();

  var ticks = 0, start = Date.now();

  // Trigger update of game according to time_step
  setInterval(function() {
    game.tick();
    ticks++;
  }, game_attributes.time_step * 1000);

  // setInterval(function() {
  //   console.log('tps = %s', (ticks / (Date.now() - start) * 1000).toFixed(2));
  // }, 1000);

  //
  // Handle SocketIO connections
  //
  io.sockets.on('connection', function(socket) {
    var player = null, leave_game;

    //
    // Messages applicable for all connected clients
    //

    socket.on('get game state', function() {
      socket.emit('game state', game.getState());
    });

    // Handle `join game`
    socket.on('join game', function(name) {
      if (player) {
        return socket.emit('join failed', 'already joined game');
      }

      // Make sure name is valid
      var nameExists = game.getPlayers().some(function(player) {
        return player.get('name') === name;
      });
      nameExists = false; // TODO Remove this debug code
      if (nameExists) {
        return socket.emit('join failed', 'name exists');
      }

      // Set a random position in game for player
      player_attributes.position.x =
        Math.random() * (game_attributes.size.width - player_attributes.size.width);
      player_attributes.position.y = 
        Math.random() * (game_attributes.size.height - player_attributes.size.height);

      // Add player to game
      player = game.addPlayer(name, player_attributes);

      // Listen for events on player
      player.on('still', function() {
        // Broadcast player still
        io.sockets.emit('still player', player.id, player.getState());
      });

      // // Send ball states every third second
      // socket.set('ball states interval', setInterval(function() {
      //   socket.emit('ball states', game.getBallStates());
      // }, 3000));

      // Emit success and player info to player
      socket.emit('game joined', player.id);

      // Add ball if there are 2 players and no balls exists
      if (game.getPlayers().length === 2 && game.getBalls().length === 0) {
        addBall();
      }
    });

    //
    // The following messages are for joined players only
    //

    socket.on('move', function(direction) {
      if (!player) return;

      // Start moving
      player.move(direction);
      // Broadcast player move
      io.sockets.emit('move player', player.id, player.getState(), direction);
    });

    socket.on('stop', function(direction) {
      if (!player) return;

      // Stop player
      player.stop();
      // Broadcast player stop
      io.sockets.emit('stop player', player.id, player.getState());
    });

    socket.on('chat message', function(content) {
      if (!player) return;

      var message = {
        player  : player.id
      , name    : player.get('name')
      , time    : Date.now()
      , content : content
      };
      messages.push(message);

      io.sockets.emit('chat message', message);
    });

    socket.on('leave game', leave_game = function() {
      if (!player) return;

      // Remove listeners from player
      player.removeAllListeners('still');

      // Remove player from game
      game.removePlayer(player.id);

      // Clear player
      player = undefined;

      // Emit response
      socket.emit('game left');
    });

    // Handle disconnect
    socket.on('disconnect', leave_game);

    // Finally, emit last 10 chat messages
    messages.slice(-10).forEach(function(message) {
      socket.emit('chat message', message);
    });

  });

  // Forward game events to clients over socket.io

  game.on('add object', function(object) {
    io.sockets.emit('add object', object.getState());
  });

  game.on('remove object', function(object) {
    io.sockets.emit('remove object', object.id);
  });

  game.on('dead ball', function(ball, edge, score) {
    io.sockets.emit('dead ball', ball.id, edge, ball.getPosition(), score);
  });

  game.on('score updated', function(score) {
    io.sockets.emit('score updated', score);
  });

  game.on('objects collided', function(o1, o2) {
    io.sockets.emit('objects collided', o1.id, o1.getState(), o2.id, o2.getState());
  });

  /**
   * Start listening
   */
  app.listen(config['--port'] || 3000);
  console.log("Server listening on port %d", app.address().port);
});