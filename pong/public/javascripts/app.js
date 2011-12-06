$(document).ready(function() {

  // Foundation setup
	$('input, textarea').placeholder();

  var Game         = require('/game')
    , game         = new Game({})
    , renderer     = new Renderer($('#game'), game, { arrows:true })
    , selfId       = null
    , tickloop     = null;

  var socket = io.connect()
    .on('connect', function() {
      notify('info', 'Connected to server');

      // Load initial state
      socket.emit('get game state');

      // Show game
      renderer.show();

      // Start tickloop
      if (!tickloop) {
        tickloop = function() {
          if (tickloop) requestAnimFrame(tickloop, $('#game'));
          game.tick();
          renderer.gameTicked();
        };
        tickloop();
      }

      updateUI(false);
    })
    .on('connect_failed', function() {
      $('#signup_action').attr('disabled',true).val('Could not connect');
      notify('error', 'Failed to connect to server...');
    })
    .on('disconnect', function() {
      tickloop = null;
      renderer.hide();
      game.resetGameState();
      $('#signup_action').attr('disabled',true);
      notify('info', 'Disconnected from server');
    })
    // Handle join/leave of game
    .on('game joined', function(id) {
      $('#signup').trigger('reveal:close');

      selfId = id;
      renderer.setSelf(selfId);

      updateUI(true);
    })
    .on('game left', function() {
      selfId = undefined;
      renderer.setSelf(selfId);
      updateUI(false);
    })
    .on('join failed', function(message) {
      $('#signup_action').attr('disabled',false);
      if (message = 'name exists') {
        $('#signup_error').text('Player with name "' + name + '" already exists.').show();
      } else {
        $('#signup_error').text('Server error: ' + message).show();
      }
    })

    .on('game state', function(state) {
      game.updateState(state);
      renderer.gameStateUpdated();
    })
    // .on('ball states', function(states) {
    //   game.updateObjectStates(states);
    // })
    .on('add object', function(state) {
      game.addObject(state);
    })
    .on('remove object', function(id) {
      game.removeObject(id);
    })
    .on('dead ball', function(id, edge, position, score) {
      var ball = game.getBall(id);
      if (ball) {
        ball.setPosition(position);
        renderer.objectStateUpdated(ball);
        ball.die(edge);
      }
      renderer.ballDead(ball, edge, score);
    })
    .on('objects collided', function(o1_id, o1_state, o2_id, o2_state) {
      var o1 = game.getObject(o1_id);
      if (o1) {
        o1.updateState(o1_state);
        renderer.objectStateUpdated(o1);
      }
      var o2 = game.getObject(o2_id);
      if (o2) {
        o2.updateState(o2_state);
        renderer.objectStateUpdated(o2);
      }
      if (o1 && o2) {
        renderer.objectsCollided(o1, o2);
      }
    })
    .on('move player', function(id, state, direction) {
      var player = game.getPlayer(id);
      if (player) {
        player.updateState(state);
        renderer.objectStateUpdated(player);
        player.move(direction);
      }
    })
    .on('stop player', function(id, state) {
      var player = game.getPlayer(id);
      if (player) {
        player.stop();
        player.updateState(state);
        renderer.objectStateUpdated(player);
      }
    })
    .on('still player', function(id, state) {
      var player = game.getPlayer(id);
      if (player) {
        player.updateState(state);
        renderer.objectStateUpdated(player);
      }
    })
    .on('score updated', function(score) {
      renderer.scoreUpdated(score);
    })
    .on('chat message', function(message) {
      var elem = $('<li class="message" />')
        , time = new Date(message.time)
        , dateString = time.getDate() + '/' + (time.getMonth() + 1);
      elem.append($('<span class="info" />').text(dateString + ' ' + formatTime(time) + ' - '));
      elem.append($('<span class="name" />').text(message.name + ': '));
      elem.append($('<span class="content" />').text(message.content));
      $('#chat .messages').append(elem).scrollToBottom();
    });

  socket.socket.on('error', function (reason) {
    alert('SocketIO error: ' + reason);
  });

  //
  // UI code
  //

  var updateUI = function(joined) {
    $('#legend')[joined ? 'show' : 'hide']();
    $('#join_game')[joined ? 'hide' : 'show']();
    $('#leave_game')[joined ? 'show' : 'hide']();
    $('#chat_form')[joined ? 'show' : 'hide']();
  };

  Notify($("#notify"));
  var notify = function(type, message) {
    $.notify[type](message);
  };

  $('#join_game').click(function(event) {
    event.preventDefault();
    $('#signup_action').attr('disabled',false);
    $('#signup')
      .find('.error').hide().end()
      .reveal({
      })
      .find('input[name="name"]').focus();
  });

  $('#leave_game').click(function(event) {
    event.preventDefault();
    socket.emit('leave game');
  });

  $('#signup').find('form').submit(function(event) {
    event.preventDefault();

    var name  = $('input[name="name"]', this).val()
      , error = $('.error', this);

    if (/^\s*$/.test(name)) {
      return error.text('Must supply name').show();
    }

    $('#signup_action').attr('disabled', true);
    socket.emit('join game', name);
  });

  $('#chat').
    find('input[name="message"]').keydown(function(event) {
      if (event.keyCode === 13) { // Enter
        event.preventDefault();
        socket.emit('chat message', $(this).val());
        $(this).val('');
      }
    }).end().
    find('.send').click(function(ev) {
      ev.preventDefault();
      var input = $('input[name="message"]', '#chat');
      socket.emit('chat message', input.val());
      input.val('');
    }).end();

  var moving = false
    , keys   = {};

  var directions = {
    '37' : { x:-1, y: 0 } // left
  , '38' : { x: 0, y:-1 } // up
  , '39' : { x: 1, y: 0 } // right
  , '40' : { x: 0, y: 1 } // down
  , '37_38' : { x:-1, y:-1 } // left-up
  , '37_40' : { x:-1, y: 1 } // left-down
  , '38_39' : { x: 1, y:-1 } // up-right
  , '39_40' : { x: 1, y: 1 } // right-down
  };

  $(document).keydown(function(ev) {
    if (ev.keyCode in directions) {
      keys[ev.keyCode] = true;
      var key = _.keys(keys).sort().join('_');
      if (key in directions) {
        socket.emit('move', directions[key]);
        moving = true;
      }
      ev.preventDefault();
    };
  }).keyup(function(ev) {
    if (ev.keyCode in directions) {
      delete keys[ev.keyCode];
      var key = _.keys(keys).sort().join('_');
      if (key in directions) {
        socket.emit('move', directions[key]);
        moving = true;
      } else if (moving) {
        socket.emit('stop');
        moving = false;
      }
      ev.preventDefault();
    };
  });

  $('#game #board').bind('dblclick', function(ev) {
    ev.preventDefault();
    // Move player towards mouse position
    movePlayerToMouse($(this), ev);
  }).bind('click', function(ev) {
    ev.preventDefault();
    // Stop moving on single click
    if (moving) {
      socket.emit('stop');
      moving = false;
    }
  }).bind('touchstart', function(ev) {
    // Move player towards touch position
    movePlayerToMouse($(this), ev);
    // Start listening for move events
    $(this).bind('touchmove', mouseMoveEventHandler);
  }).bind('touchend touchcancel touchleave', function() {
    // Stop listening for move events
    $(this).unbind('touchmove', mouseMoveEventHandler);
    if (moving) {
      socket.emit('stop');
      moving = false;
    }
  });

  // debounce a couple of ms before invoking
  var mouseMoveEventHandler = _.debounce(function(ev) {
    if (moving) {
      // Only update direction if moving, otherwise we stopped during the debouncing
      movePlayerToMouse($(this), ev);
    }
  }, 100);

  var movePlayerToMouse = function(parent, ev) {
    var player = game.getPlayer(selfId);
    if (player) {
      ev.preventDefault();

      // Handle touch input
      if (ev.changedTouches && ev.changedTouches.length > 0) {
        ev = ev.changedTouches[0];
        // Taken from jQuery to calculate pageX/Y from clientX/Y
        if (ev.pageX == null && ev.clientX != null) {
	        var doc  = document.documentElement
	          , body = document.body;

          ev.pageX = ev.clientX + (doc && doc.scrollLeft || body && body.scrollLeft || 0) - (doc && doc.clientLeft || body && body.clientLeft || 0);
          ev.pageY = ev.clientY + (doc && doc.scrollTop  || body && body.scrollTop  || 0) - (doc && doc.clientTop  || body && body.clientTop  || 0);
        }
      }

      // Extract position
      var parent_offset = parent.offset()
        , position      = { x: ev.pageX - parent_offset.left, y: ev.pageY - parent_offset.top };

      // Get game positions
      var mouse    = renderer.cssToGame(position)
        , current  = player.getPosition();

      // Emit move
      socket.emit('move', { x: mouse.x - current.x, y: mouse.y - current.y });
      moving = true;
    }
  };

  //
  // Utility functions
  //

  // Simple scroll functionality in jQuery
  $.fn.scrollToBottom = function() {
    this.prop('scrollTop', this.prop('scrollHeight'));
  };

  // shim layer with setTimeout fallback
  var requestAnimFrame = (function() {
    // return function(callback, elem){
    //   window.setTimeout(callback, 1000 / 20);
    // };
    return  window.requestAnimationFrame       ||
            window.webkitRequestAnimationFrame ||
            window.mozRequestAnimationFrame    ||
            window.oRequestAnimationFrame      ||
            window.msRequestAnimationFrame     ||
            function(callback, elem){
              window.setTimeout(callback, 1000 / 60);
            };
  })();

  var formatDate = function(date) {
    if (!date) return '';
    return [date.getFullYear(), padNumber(date.getMonth() + 1, 2), padNumber(date.getDate(), 2)].join('-');
  };

  var formatTime = function(date) {
     return [padNumber(date.getHours(), 2), padNumber(date.getMinutes(), 2), padNumber(date.getSeconds(), 2)].join(':');
  };

  var padNumber = function(value, length) {
    var string = value.toString();
    if (length <= string.length) return string.substring(0, length);
    return Array(length + 1 - string.length).join('0') + string;
  };

});
