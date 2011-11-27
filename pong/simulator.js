var io = require('socket.io-client');

var clientIds = 0
  , timespans = {
      connect : { min: 0.1, max: 2 }
    , join    : { min: 1,   max: 5   }
    , wait    : { min: 1,   max: 3   }
    , move    : { min: 0.5, max: 1.5 }
    , chat    : { min: 10,  max: 60  }
    , leave   : { min: 120, max: 180 }
    };

var delay = function(timespan, fn) {
  return setTimeout(fn, (timespan.min + Math.random() * (timespan.max - timespan.min)) * 1000);
};

var stopDelay = function(ref) {
  clearTimeout(ref);
};

var repeat = function(fn) {
  process.nextTick(function() {
    fn(function() { repeat(fn); });
  });
};

var connect_client = function(hostname, debug) {
  var clientId  = ++clientIds
    , joinCount = 0
    , playerId  = null;

  var socket = io.connect(hostname, { 'force new connection':true })
    .on('connect', function() {
      if (debug) console.log('Client[%d]: Connected', clientId);
      socket.emit('get game state');
      delay(timespans.join, function() {
        socket.emit('join game', 'Sim_' + clientId);
      });
    })
    .on('connect_failed', function() {
      if (debug) console.error('Client[%d]: Connect failed', clientId, arguments);
    })
    .on('disconnect', function() {
      if (debug) console.log('Client[%d]: Disconnected', clientId);
    })
    .on('game joined', function(id) {
      if (debug) console.log('Client[%d]: Joined game as player %d', clientId, id);
      playerId = id;

      var moveLoop, chatLoop;

      // Start move loop
      repeat(function(next) {
        moveLoop = delay(timespans.wait, function() {
          if (playerId == null) return;
          socket.emit('move', { x:Math.random() - 0.5, y:Math.random() - 0.5 });
          moveLoop = delay(timespans.move, function() {
            if (playerId == null) return;
            socket.emit('stop');
            next();
          });
        });
      });

      // Start chat loop
      repeat(function(next) {
        chatLoop = delay(timespans.chat, function() {
          if (playerId == null) return;
          socket.emit('chat message', 'Message from simulator ' + clientId);
          next();
        });
      });

      // Start leave timer
      delay(timespans.leave, function() {
        stopDelay(moveLoop);
        stopDelay(chatLoop);
        socket.emit('leave game');
      });
    })
    .on('game left', function() {
      if (debug) console.log('Client[%d]: Left game', clientId);
      playerId = undefined;
      delay(timespans.join, function() {
        socket.emit('join game', 'Sim_' + clientId + '_' + ++joinCount);
      });
    })
    .on('join failed', function(message) {
      if (debug) console.log('Client[%d]: Failed to join game: %s', clientId, message);
    })
    .on('game state', function(state) {
      if (debug > 1) console.log('Client[%d]: Received game state: %o', clientId, state);
    })
    .on('ball states', function(states) {
      if (debug > 1) console.log('Client[%d]: Received ball states: %o', clientId, states);
    })
    .on('add object', function(state) {
      if (debug > 1) console.log('Client[%d]: Received "add object": %o', clientId, state);
    })
    .on('remove object', function(id) {
      if (debug > 1) console.log('Client[%d]: Received "remove object": %o', clientId, id);
    })
    .on('dead ball', function(id, edge, position, score) {
      if (debug > 1) console.log('Client[%d]: Received "dead ball": %o', clientId, arguments);
    })
    .on('objects collided', function(o1_id, o1_state, o2_id, o2_state) {
      if (debug > 1) console.log('Client[%d]: Received "objects collided": %o', clientId, arguments);
    })
    .on('move player', function(id, state, direction) {
      if (debug > 1) console.log('Client[%d]: Received "move player": %o', clientId, arguments);
    })
    .on('stop player', function(id, position) {
      if (debug > 1) console.log('Client[%d]: Received "stop player": %o', clientId, arguments);
    })
    .on('chat message', function(message) {
      if (debug > 2) console.log('Client[%d]: Received "chat message": %o', clientId, arguments);
    });

  socket.socket.on('error', function (reason) {
    if (debug) console.error('Client[%d]: Socket error: %s', clientId, reason);
  });
};

var hostname = process.argv[2]
  , clients  = parseInt(process.argv[3], 10)
  , debug    = parseInt(process.argv[4], 10);

if (isNaN(debug)) debug = false;

if (isNaN(clients)) {
  console.log('Usage: node %s <hostname> <number of clients> [debug]', process.argv[1]);
} else {
  console.log('Starting %d clients (using debug "%s")', clients, debug);
  repeat(function(next) {
    delay(timespans.connect, function() {
      console.log('Connecting next client');
      connect_client(hostname, debug);
      if (--clients > 0) next();
    });
  });
}