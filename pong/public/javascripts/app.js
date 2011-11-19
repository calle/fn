$(document).ready(function() {

  // Foundation setup
	$('input, textarea').placeholder();

  //
  // UI code
  //

  var signupEl = $('#signup')
    , gameEl   = $('#game');

  var show_login = function() {
    signupEl.find('.error').hide();
    signupEl.reveal({
      closeOnBackgroundClick : false
    });
  };

  signupEl.find('form').submit(function() {
    var name = signupEl.find('input[name="name"]').val();
    if (/^\s*$/.test(name)) {
      signupEl.find('.error').text('Must supply name').show();
    } else {
      joinGame(name, function(err) {
        if (err) {
          if (err.message = 'name exists') {
            signupEl.find('.error').text('Player with name "' + name + '" already exists.').show();
          } else {
            signupEl.find('.error').text('Server error: ' + err.message).show();
          }
        } else {
          signupEl.trigger('reveal:close');
        }
      });
    }
    return false;
  });

  var moving = false;
  $(document).keydown(function(ev) {
    if (moving) return;

    if (ev.keyCode === 38) {
      console.log('move up');
      socket.emit('move', 'up');
      moving = true;
    } else if (ev.keyCode === 40) {
      console.log('move down');
      socket.emit('move', 'down');
      moving = true;
    }
  }).keyup(function(ev) {
    if (ev.keyCode === 38 || ev.keyCode === 40) {
      console.log('stop moving');
      socket.emit('stop');
      moving = false;
    }
  });

  //
  // Game code
  //

  var Game         = require('/game')
    , game         = new Game({})
    , renderer     = new Renderer(gameEl, game)
    , tickloop     = null
    , selfId       = null;

  var joinGame = function(name, cb) {
    socket.emit('join game', name, function(err, id) {
      if (err) return cb(err);

      cb();

      selfId = id;
      renderer.setSelf(id);
      renderer.show();

      // Start tickloop
      if (!tickloop) {
        tickloop = function() {
          if (tickloop) requestAnimFrame(tickloop, $('#game'));
          game.tick();
        };
        tickloop();
      }

      // Load initial state
      socket.emit('game state', function(err, state) {
        console.log('got game state, %o', state);
        game.updateState(state);
      });

    });    
  };

  //
  // Socket.IO
  //

  var socket = io.connect();

  socket.on('game state', function(state) {
    game.updateState(state);
  });

  socket.on('object states', function(states) {
    game.updateObjectStates(states);
  });

  socket.on('add object', function(state) {
    game.addObjectByState(state);
  });

  socket.on('remove object', function(id) {
    game.removeObject(id);
  });

  socket.on('ball dead', function(id, edge) {
    var ball = game.getBall(id);
    if (ball) ball.die(edge);
  });

  socket.on('move player', function(id, position, direction) {
    var player = game.getPlayer(id);
    if (player) {
      player.setPosition(position);
      player.move(direction);
    }
  });

  socket.on('stop player', function(id, position) {
    var player = game.getPlayer(id);
    if (player) {
      player.stop();
      player.setPosition(position);
    }
  });

  socket.on('disconnect', function() {
    tickloop = null;
    renderer.hide();
  });

  //
  // Show initial form
  //

  show_login();

  //
  // Utility functions
  //

  // shim layer with setTimeout fallback
  var requestAnimFrame = (function() {
    return  window.requestAnimationFrame       || 
            window.webkitRequestAnimationFrame || 
            window.mozRequestAnimationFrame    || 
            window.oRequestAnimationFrame      || 
            window.msRequestAnimationFrame     || 
            function(callback, elem){
              window.setTimeout(callback, 1000 / 60);
            };
  })();

});