var GameObject = require('./gameobject');

/**
 * Ball has the following attributes (in addition to GameObject's)
 *  - speed
 *  - dead
 */
var Ball = module.exports = function(id, attributes) {
  GameObject.call(this, id, Ball.TYPE, attributes);

  // Set velicoty using speed
  var speed = this.get('speed')
    , angle = Math.random() * (Math.PI / 2) - (Math.PI / 4); // angle between -45 and 45 degrees

  this.setVelocity({
    x : Math.cos(angle) * speed
  , y : Math.sin(angle) * speed
  });

  console.log('>>> Angle: %f => %s', angle, JSON.stringify(this.getVelocity()));

  // Always start out alive
  this.set('dead', false);
};

Ball.TYPE = 'ball';

GameObject.extend(Ball);

Ball.prototype.die = function(edge) {
  if (!this.get('dead')) {
    this.set('dead', edge);
    this.setVelocity({ x:0, y: 0 });
    this.emit('dead', edge);
  }
};

Ball.prototype.updateState = function(state) {
  var dead = this.get('dead');

  // Invoke super to update state
  GameObject.prototype.updateState.apply(this, arguments);

  // Check if we died, then emit (dead and velocity is already set correct by state)
  if (!dead && state.dead) {
    this.emit('dead', state.dead);
  }
};

Ball.prototype._validatePosition = function(position, velocity, duration, game) {
  var self    = this
    , geom    = this.getGeometry()
    , board   = game.getBoard()
    , players = game.getPlayers();

  // Bounce off top and bottom walls
  if (geom.bottom >= board.bottom) {
    console.log('>>> bounce ball at bottom');
    position.y = Math.min(board.bottom - geom.height, position.y - velocity.y * duration);
    velocity.y = -velocity.y;
  } else if (geom.top <= board.top) {
    console.log('>>> bounce ball at top');
    position.y = Math.max(board.top, position.y - velocity.y * duration);
    velocity.y = -velocity.y;
  }

  // TODO: Use player width here to determine when to look for collitions

  // See if we are outside board on either left or right side
  if (geom.left <= board.left) {
    if (_.some(players, this._overlapsObject)) {
      position.x = Math.max(board.left, position.x - velocity.x * duration);
      velocity.x = -velocity.x;
    } else {
      this.die('left');
    }
  } else if (geom.right >= board.right) {
    if (_.some(players, this._overlapsObject)) {
      position.x = Math.min(board.right - geom.width, position.x - velocity.x * duration);
      velocity.x = -velocity.x;
    } else {
      this.die('right');
    }
  }
};