var Renderer = function(elem, game, options) {
  this.elem = elem;
  this.game = game;

  this.elem.board   = this.elem.find('#board');
  this.elem.score   = this.elem.find('#score');

  this.css_scale = { x: 1, y: 1 };

  _.bindAll(this, '_addPlayer', '_removePlayer', '_addBall', '_removeBall', '_updateObjectGeometry');

  // Setup listeners on game for simple events
  this.game.on('add player',    this._addPlayer);
  this.game.on('remove player', this._removePlayer);
  this.game.on('add ball',      this._addBall);
  this.game.on('remove ball',   this._removeBall);

  // // Update FPS every second
  // var fps = this.elem.append('<div id="fps" />').find('#fps');
  // setInterval(function() {
  //   fps.text(game.fps + ' fps');
  // }, 1000);
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

Renderer.prototype.gameStateUpdated = function() {
  // Update css scale based on game board size
  this._updateCssScale();
  this._updateScore(this.game.getScore());
};

Renderer.prototype.objectStateUpdated = function(object) {
  // // Flash in red to singal update from server
  // if (object.elem) flashElement(object.elem, 'red');
};

Renderer.prototype.objectsCollided = function(o1, o2) {
  // // Flash in blue to singal update from server
  // if (o1.elem) flashElement(o1.elem, 'blue');
  // if (o2.elem) flashElement(o2.elem, 'blue');
};

Renderer.prototype.ballDead = function(ball, edge, score) {
  // console.log('Renderer: Ball %d dead at %s, %o', ball && ball.id, edge, arguments);

  if (ball && ball.elem) ball.elem.addClass('dead');

  // Update score
  this._updateScore(score);

  // Flash board in red
  flashElement(this.elem.board, '#600');
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
  // console.log('Renderer._updateScore(%s)', JSON.stringify(score));
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

  // console.log('Renderer: set css_scale using %dx%d and %s => %s',
  //   board_elem.width(), board_elem.height(),
  //   JSON.stringify(board), JSON.stringify(this.css_scale));

  // Reposition all game objects after changing css_scale
  _.each(this.game.getObjects(), this._updateObjectGeometry);
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
  this._updateObjectGeometry(player);
  // console.log('Renderer: created player "%s"', player.get('name'));

  // Listen for player updates
  player.on('move',   this._updateObjectGeometry);
  player.on('resize', this._updateObjectGeometry);
};

Renderer.prototype._removePlayer = function(player) {
  // console.log('Renderer._removePlayer(%d)', player.id);

  // Remove player elem
  player.elem.remove();
  delete player.elem.info;
  delete player.elem;

  // Remove listeners
  player.removeListener('move',   this._updateObjectGeometry);
  player.removeListener('resize', this._updateObjectGeometry);
};

Renderer.prototype._addBall = function(ball) {
  // Create ball elem
  ball.elem = $('<div class="ball" />');
  ball.elem.attr('id', 'ball-' + ball.id);

  // Add element
  this.elem.board.append(ball.elem);
  this._updateObjectGeometry(ball);
  // console.log('Renderer: created ball %d', ball.id);

  // Listen for ball updates
  ball.on('move',   this._updateObjectGeometry);
  ball.on('resize', this._updateObjectGeometry);
};

Renderer.prototype._removeBall = function(ball) {
  // Remove ball elem
  ball.elem.remove();
  delete ball.elem;

  // Remove listeners
  ball.removeListener('move',   this._updateObjectGeometry);
  ball.removeListener('resize', this._updateObjectGeometry);
};

Renderer.prototype._updateObjectGeometry = function(object) {
  var last         = object.css_geometry
    , css_geometry = this._cssGeometry(object.getGeometry());
  if (!last || !_.isEqual(last, css_geometry)) {
    var radius = (css_geometry.width / 2) + 'px / ' + (css_geometry.height / 2) + 'px';
    object.elem.css({
      top    : css_geometry.top
    , left   : css_geometry.left
    , width  : css_geometry.width
    , height : css_geometry.height
    , '-webkit-border-radius' : radius
    , '-moz-border-radius'    : radius
    , 'border-radius'         : radius
    });
    if (object.elem.info) {
      object.elem.info.css({
        left : css_geometry.width + 4
      , top  : 2
      });
    }
  }
  object.css_geometry = css_geometry;
};

Renderer.prototype._cssGeometry = function(geometry) {
  return {
    left   : Math.round(geometry.left   * this.css_scale.x)
  , right  : Math.round(geometry.right  * this.css_scale.x)
  , top    : Math.round(geometry.top    * this.css_scale.y)
  , bottom : Math.round(geometry.bottom * this.css_scale.y)
  , width  : Math.round(geometry.width  * this.css_scale.x)
  , height : Math.round(geometry.height * this.css_scale.y)
  };
};

//
// Utilities
//

var flashElement = function(elem, color) {
  // Complete existing animation quickly
  elem.stop(true, true);

  // Animate background color
  var before = elem.css('backgroundColor');
  elem.animate({ backgroundColor: color  }, 'fast');
  elem.animate({ backgroundColor: before }, 'fast');
};