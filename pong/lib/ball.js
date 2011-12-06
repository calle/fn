var GameObject = require('./gameobject');

/**
 * Ball has the following attributes (in addition to GameObject's)
 *  - speed
 *  - dead
 */
var Ball = module.exports = function(id, attributes) {
  GameObject.call(this, id, Ball.TYPE, attributes);

  // Set velicoty using angle between -45 and 45 degrees
  var angle = Math.random() * (Math.PI / 2) - (Math.PI / 4)
    , speed = this.get('speed');
  this.setVelocity({
    x : Math.cos(angle) * speed
  , y : Math.sin(angle) * speed
  });

  // Always start out alive
  this.set('dead', false);
};

Ball.TYPE = 'Ball';

GameObject.extend(Ball);

Ball.prototype.die = function(edge) {
  if (!this.get('dead')) {
    this.setVelocity({ x:0, y: 0 });
    this.set('dead', edge);
    this.emit('dead', this, edge);
  }
};

Ball.prototype.updateState = function(state) {
  var previousDead = this.get('dead');

  // Invoke super to update state
  GameObject.prototype.updateState.apply(this, arguments);

  // Check if we died, then emit (dead and velocity is already set correct by state)
  if (!previousDead && this.get('dead')) {
    this.emit('dead', this, this.get('dead'));
  }
};

Ball.prototype.setVelocity = function(velocity) {
  if (this.get('dead')) return;
  GameObject.prototype.setVelocity.apply(this, arguments);
};

Ball.prototype._validCollisionTarget = function(other) {
  return !this.get('dead');
};

Ball.prototype._updateObject = function(position, velocity, duration, game) {
  // Invoke super method to update object
  GameObject.prototype._updateObject.apply(this, arguments);

  // Check collisions against walls
  var geom    = this.getGeometry()
    , board   = game.getBoard();

  if (geom.bottom > board.bottom) {
    // Bounce on bottom wall
    velocity.y = -velocity.y;
    position.y = board.bottom - geom.height;
  } else if (geom.top < board.top) {
    // Bounce on top wall
    velocity.y = -velocity.y;
    position.y = board.top;
  } else if (geom.left <= board.left) {
    // Die on left wall
    this.die('left');
  } else if (geom.right >= board.right) {
    // Die on right wall
    this.die('right');
  }
};
