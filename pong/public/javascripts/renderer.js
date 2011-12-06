var Renderer = function(elem, game, options) {
  this.elem = elem;
  this.game = game;

  this.elem.board   = this.elem.find('#board');
  this.elem.score   = this.elem.find('#score');

  this.css_scale = { x: 1, y: 1 };

  _.bindAll(this, '_addPlayer', '_removePlayer', '_addBall', '_removeBall',
    '_updateObjectSize', '_updateObjectPosition');

  // Setup listeners on game for simple events
  this.game.on('add player',    this._addPlayer);
  this.game.on('remove player', this._removePlayer);
  this.game.on('add ball',      this._addBall);
  this.game.on('remove ball',   this._removeBall);

  if (options.fps) {
    // Update FPS every second
    var fps = this.elem.append('<div id="fps" />').find('#fps');
    setInterval(function() {
      fps.text(game.fps + ' fps');
    }, 1000);
  }

  this.arrows = options.arrows;
  if (this.arrows) {
    this.canvas = this.elem.board.append('<canvas />').find('canvas');
    if (this.canvas[0].getContext('2d')) {
      this.canvas.css({
        position  : 'absolute'
      , top       : 0
      , left      : 0
      , width     : '100%'
      , height    : '100%'
      , 'z-index' : 1
      });
    } else {
      // No canvas support
      this.canvas = null;
      this.arrows = false;
    }
  }
};

Renderer.prototype.show = function() {
  // Show element
  this.elem.show();
  // Update css scale since our elem size has changed
  this._updateCssScale();
};

Renderer.prototype.hide = function() {
  this.elem.hide();
};

Renderer.prototype.setSelf = function(id) {
  if (this.self) {
    this.elem.board.find('#player-' + this.self).removeClass('self');
  }
  this.self = id;
  this.elem.board.find('#player-' + id).addClass('self');
};

Renderer.prototype.gameTicked = function() {
  if (this.arrows) {
    // Draw velocity and acceleration arrows
    var self   = this
      , ctx    = this.canvas[0].getContext('2d')
      , isTiny = function(v) {
          return !v || (Math.abs(v.x) < 0.1 && Math.abs(v.y) < 0.1);
        }
      , arrow  = function(from, to) {
          // Convert to css coordinates
          from = {
            x : Math.round(from.x * self.css_scale.x)
          , y : Math.round(from.y * self.css_scale.y)
          };
          to   = {
            x : Math.round(to.x * self.css_scale.x)
          , y : Math.round(to.y * self.css_scale.y)
          };

          // Draw line
          ctx.beginPath();
          ctx.moveTo(from.x, from.y);
          ctx.lineTo(to.x, to.y);

          // Draw arrow head
          var angle  = Math.atan2(to.y - from.y, to.x - from.x)
            , arrow_length = 10;
          ctx.lineTo(
            to.x - arrow_length*Math.cos(angle-Math.PI/6),
            to.y - arrow_length*Math.sin(angle-Math.PI/6));
          ctx.moveTo(to.x, to.y);
          ctx.lineTo(
            to.x - arrow_length*Math.cos(angle+Math.PI/6),
            to.y - arrow_length*Math.sin(angle+Math.PI/6));

          ctx.closePath();
          ctx.stroke();
        };

    ctx.clearRect(0, 0, this.canvas.width(), this.canvas.height());

    var scale = 2;

    _.each(this.game.getObjects(), function(object) {
      var position = object.getPosition()
        , size     = object.getSize()
        , velocity = object.getVelocity()
        , center   = { x:position.x + size.width/2, y:position.y + size.height/2 }
        , target   = { x:center.x + velocity.x / scale, y:center.y + velocity.y / scale };

      if (isTiny(velocity)) return;

      if (object.type === 'Ball') {
        ctx.strokeStyle = '#0A6289';
        // ctx.strokeStyle = '#7BC8E9';
        arrow(center, target);
      } else {
        ctx.strokeStyle = '#07903F';
        // ctx.strokeStyle = '#79EEA8';
        arrow(center, target);

        var acceleration = object.get('movement').acceleration;
        if (isTiny(acceleration)) return;
        ctx.strokeStyle = '#A62709';
        // ctx.strokeStyle = '#FF9981';
        arrow(center, { x:center.x + acceleration.x / scale, y:center.y + acceleration.y / scale });
      }
    });
  }
};

Renderer.prototype.gameStateUpdated = function() {
  // Update css scale based on game board size
  this._updateCssScale();
  this._updateScore(this.game.getScore());
};

Renderer.prototype.objectStateUpdated = function(object) {
  // // Flash in red to singal update from server
  if (object.elem) temporarilyAddClass(object.elem, 'updated', 100);
};

Renderer.prototype.objectsCollided = function(o1, o2) {
  // // Flash in blue to singal update from server
  if (o1.elem) temporarilyAddClass(o1.elem, 'colliding', 200);
  if (o2.elem) temporarilyAddClass(o2.elem, 'colliding', 200);
};

Renderer.prototype.ballDead = function(ball, edge, score) {
  if (ball && ball.elem) ball.elem.addClass('dead');

  // Update score
  this._updateScore(score);

  // Flash board in red
  temporarilyAddClass(this.elem.board, 'lost', 200);
};

Renderer.prototype.scoreUpdated = function(score) {
  this._updateScore(score);
};

Renderer.prototype.cssToGame = function(pos) {
  return {
    x : pos.x / this.css_scale.x
  , y : pos.y / this.css_scale.y
  };
};

//
// Internal methods
//

Renderer.prototype._updateScore = function(score) {
  this.elem.score.find('.value').text(score);
};

Renderer.prototype._updateCssScale = function() {
  // Update css_scale using size of board elem
  var board = this.game.board
    , board_elem  = this.elem.board;

  // Set height of board to 0.6 x width
  board_elem.css({ height: 0.6 * board_elem.width() });
  this.css_scale = {
    x : board_elem.width()  / board.width
  , y : board_elem.height() / board.height
  };

  // Update all game objects after changing css_scale
  _.each(this.game.getObjects(), function(object) {
    this._updateObjectSize(object);
    this._updateObjectPosition(object);
  }, this);

  if (this.arrows) {
    // Update canvas width/height
    this.canvas[0].width  = board_elem.width();
    this.canvas[0].height = board_elem.height();
  }
};

Renderer.prototype._addPlayer = function(player) {
  // Create player elem
  player.elem = $('<div class="player"></div>');
  player.elem.attr('id', 'player-' + player.id);

  // Add name
  if (player.get('name') !== 'enemy') {
    player.elem.info = $('<span class="name">' + player.get('name') + '</span>');
    player.elem.append(player.elem.info);
  }

  // Check self
  if (player.id === this.self) {
    player.elem.addClass('self');
  }

  // Add element
  this.elem.board.append(player.elem);
  this._updateObjectSize(player);
  this._updateObjectPosition(player);

  // Listen for player updates
  player.on('resize', this._updateObjectSize);
  player.on('move',   this._updateObjectPosition);
};

Renderer.prototype._removePlayer = function(player) {
  // Remove player elem
  player.elem.remove();
  delete player.elem.info;
  delete player.elem;

  // Remove listeners
  player.removeListener('resize', this._updateObjectSize);
  player.removeListener('move',   this._updateObjectPosition);
};

Renderer.prototype._addBall = function(ball) {
  // Create ball elem
  ball.elem = $('<div class="ball" />');
  ball.elem.attr('id', 'ball-' + ball.id);

  // Add element
  this.elem.board.append(ball.elem);
  this._updateObjectSize(ball);
  this._updateObjectPosition(ball);

  // Listen for ball updates
  ball.on('resize', this._updateObjectSize);
  ball.on('move',   this._updateObjectPosition);
};

Renderer.prototype._removeBall = function(ball) {
  // Remove ball elem
  ball.elem.remove();
  delete ball.elem;

  // Remove listeners
  ball.removeListener('resize', this._updateObjectSize);
  ball.removeListener('move',   this._updateObjectPosition);
};

Renderer.prototype._updateObjectSize = function(object) {
  var size     = object.getSize()
    , css_size = {
        width  : Math.round(size.width  * this.css_scale.x)
      , height : Math.round(size.height * this.css_scale.y)
      };
  // Calculate css radius
  var radius = (css_size.width / 2) + 'px / ' + (css_size.height / 2) + 'px';

  // Update style
  object.elem.css({
    width                   : css_size.width
  , height                  : css_size.height
  , '-webkit-border-radius' : radius
  , '-moz-border-radius'    : radius
  , 'border-radius'         : radius
  });

  // Optionaliiy update position of info element
  if (object.elem.info) {
    object.elem.info.css({
      left : css_size.width + 4
    , top  : 2
    });
  }
};

Renderer.prototype._updateObjectPosition = function(object) {
  var position     = object.getPosition()
    , css_position = {
        x : Math.round(position.x * this.css_scale.x)
      , y : Math.round(position.y * this.css_scale.y)
      }
    , last         = object.css_position;

  if (!last || !_.isEqual(last, css_position)) {
    object.elem.css({
      left : css_position.x
    , top  : css_position.y
    });
  }

  object.css_position = css_position;
};

//
// Utilities
//

var temporarilyAddClass = function(elem, cssClass, timeout) {
  var data_key = 'temp_classes';

  if (typeof timeout !== 'number') timeout = 200;

  // Get data or update elem with data
  var data = elem.data(data_key);
  if (!data) {
    elem.data(data_key, data = {});
  };

  if (cssClass in data) {
    // Stop existing timeout
    clearTimeout(data[cssClass].timeout);
  } else {
    // Add info to data and add cssClass
    data[cssClass] = {};
    elem.addClass(cssClass);
  }

  // Set timeout to remove class and save to data
  data[cssClass].timeout = setTimeout(function() {
    elem.removeClass(cssClass);
    delete data[cssClass];
  }, timeout);
};
