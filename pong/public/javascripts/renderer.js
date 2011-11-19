var Renderer = function(elem, game, options) {
  this.elem = elem;
  this.game = game;

  this.elem.board   = this.elem.find('#board');
  this.elem.players = this.elem.find('#players');
  this.elem.score   = this.elem.find('#score');

  this.css_scale = { x: 1, y: 1 };

  _.bindAll(this, 
    '_updateObjectGeometry', 
    'addPlayer', 'removePlayer',
    'addBall', 'removeBall', 'ballDead',
    'scoreUpdated');

  // Setup listeners on game
  this.game.on('add player', this.addPlayer);
  this.game.on('remove player', this.removePlayer);
  this.game.on('add ball', this.addBall);
  this.game.on('remove ball', this.removeBall);
  this.game.on('ball dead', this.ballDead);
  this.game.on('score updated', this.scoreUpdated);

  // // Debug
  // var self = this;
  // setInterval(function() {
  //   _.each(self.game.getObjects(), function(object) {
  //     var geom = object.getGeometry();
  //     console.log('%s is at %s (in css %s)', 
  //       object.type,
  //       JSON.stringify(geom), 
  //       JSON.stringify(self._cssGeometry(geom)));
  //   });
  // }, 1000);
};

Renderer.prototype.show = function() {
  // Show element
  this.elem.show();

  // Update css_scale using size of board elem
  var board = this.game.board
    , board_elem  = this.elem.board;

  this.css_scale = {
    x : board_elem.width()  / board.width
  , y : board_elem.height() / board.height
  };

  console.log('set css_scale using %dx%d => %s',
    board_elem.width(), board_elem.height(), JSON.stringify(this.css_scale));
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

Renderer.prototype.addPlayer = function(player) {
  // Create player elem
  player.elem = $('<div class="player"></div>');
  player.elem.attr('id', 'player-' + player.id);

  // Add name
  if (player.get('name') !== 'enemy') {
    player.elem.info = $('<span class="name">' + player.get('name') + '</span>');
    player.elem.append(player.elem.info);
  }

  this.elem.board.append(player.elem);
  this._updateObjectGeometry(player);

  console.log('created player "%s"',
    player.get('name'));

  if (player.id === this.self) {
    player.elem.addClass('self');
  }

  // Listen for player updates
  player.on('move', this._updateObjectGeometry);
  player.on('state updated', this._updateObjectGeometry);
};

Renderer.prototype.removePlayer = function(player) {
  // Remove player elem
  player.elem.remove();
  delete player.elem;

  // Remove listeners
  player.removeListener('move', this._updateObjectGeometry);
  player.removeListener('state updated', this._updateObjectGeometry);
};

Renderer.prototype.addBall = function(ball) {
  // Create ball elem
  ball.elem = $('<div class="ball" />');
  ball.elem.attr('id', 'ball-' + ball.id);

  this.elem.board.append(ball.elem);
  this._updateObjectGeometry(ball);

  console.log('created ball %d', ball.id);

  // Listen for ball updates
  ball.on('move', this._updateObjectGeometry);
  ball.on('state updated', this._updateObjectGeometry);
};

Renderer.prototype.removeBall = function(ball) {
  // Remove ball elem
  ball.elem.remove();
  delete ball.elem;

  // Remove listeners
  ball.removeListener('move', this._updateObjectGeometry);
  ball.removeListener('state updated', this._updateObjectGeometry);
};

Renderer.prototype.ballDead = function(ball, edge, score) {
  if (edge === 'right') {
    var board  = this.elem.board
      , before = board.css('background-color');
    board.animate({
      backgroundColor : '#f00'
    }, function() {
      board.animate({
        backgroundColor: before
      });
    });
  }
};

Renderer.prototype.scoreUpdated = function(score) {
  console.log('scoreUpdated');
  this.elem.score.find('.score').text('Score: ' + JSON.stringify(score));
};

Renderer.prototype._updateObjectGeometry = function(object) {
  var last         = object.css_geometry
    , css_geometry = this._cssGeometry(object.getGeometry());

  if (!last || !_.isEqual(last, css_geometry)) {
    if (last && last.left === css_geometry.left && last.top === css_geometry.top) {
      console.log('Should not be updated at %d, %d', css_geometry.left, css_geometry.top);
    }
    // console.log('move object(%s) to %s', object.id, JSON.stringify(css_geometry));
    object.elem.css({
      top    : css_geometry.top
    , left   : css_geometry.left
    , width  : css_geometry.width
    , height : css_geometry.height
    });

    if (object.elem.info) {
      object.elem.info.css({
        left : css_geometry.width + 10
      // , top  : css_geometry.height / 2 - object.elem.info.height() / 2
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