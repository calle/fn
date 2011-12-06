$(document).ready(function() {

  // Foundation setup
	$('input, textarea').placeholder();

  var game_attributes = {
    size      : { width: 10, height: 6 }
  , time_step : 1/30
  };
  var ball_attributes = {
    position : { x: 2, y: 5 }
  , size     : { width: 0.1, height: 0.1 }
  , weight   : 1
  , speed    : 1
  };
  var player_attributes = {
    position : { x: game_attributes.size.width - 0.1, y: 0 }
  , size     : { width: 0.3, height: 0.3 }
  , weight   : 25
  , speed    : 2
  , movement : { friction: 0.9, force: 3 }
  };

  var Game         = require('/game')
    , game         = new Game(game_attributes)
    , renderer     = new Renderer($('#game'), game, { arrows:true });

  // Add game objects

  var objects = {};

  player_attributes.position.x = Math.random() * (game_attributes.size.width - player_attributes.size.width);
  player_attributes.position.y = Math.random() * (game_attributes.size.height - player_attributes.size.height);
  objects['player1'] = game.addPlayer('Player1', player_attributes);

  player_attributes.position.x = Math.random() * (game_attributes.size.width - player_attributes.size.width);
  player_attributes.position.y = Math.random() * (game_attributes.size.height - player_attributes.size.height);
  objects['player2'] = game.addPlayer('Player2', player_attributes);

  objects['ball']    = game.addBall(ball_attributes);

  // Override Date.now
  var now = Date.now();
  Date.now = function() {
    return now;
  };

  //
  // UI code
  //

  Notify($("#notify"));
  var notify = function(type, message) {
    $.notify[type](message);
  };

  $('.actions .tick').click(function(ev) {
    ev.preventDefault();
    game.tick();
    renderer.gameTicked();

    // Step now
    now += 33;
  });

  $('#mouse_control input:radio[name="object"]').bind('click change', function(ev) {
    var object = objects[$('#mouse_control input:radio[name="object"]:checked').val()];

    if (object) {
      $('#object_info').text(JSON.stringify(object.getState(), null, 2));
    }
  });

  $('#game #board').bind('click', function(ev) {
    ev.preventDefault();

    // What to change
    var object = objects[$('#mouse_control input:radio[name="object"]:checked').val()];
    var action = $('#mouse_control input:radio[name="action"]:checked').val();

    if (object) {
      // Extract mouse position
      var parent_offset = $(this).offset()
        , position      = { x: ev.pageX - parent_offset.left, y: ev.pageY - parent_offset.top };

      // Get in game coordinates
      var mouse   = renderer.cssToGame(position)
        , current = object.getPosition();

      if (action === 'position') {
        object.setPosition(mouse);
      } else if (action === 'velocity') {
        object.setVelocity({ x:(mouse.x - current.x)/2, y:(mouse.y - current.y)/2 });
      } else if (action === 'acceleration') {
        var movement = object.get('movement');
        if (movement) {
          movement.acceleration.x = (mouse.x - current.x)/2;
          movement.acceleration.y = (mouse.y - current.y)/2;
        }
      }

      // Update renderer
      renderer.gameTicked();

      // Update info on object
      $('#object_info').text(JSON.stringify(object.getState(), null, 2));
    }    
  });

  //
  // Setup
  //

  notify('info', 'Starting simulation');

  // Show game
  renderer.show();
  renderer.gameTicked();

  // Select player1 and position
  $('#mouse_control input:radio[name="object"][value="player1"]').click();
  $('#mouse_control input:radio[name="action"][value="position"]').click();
});
