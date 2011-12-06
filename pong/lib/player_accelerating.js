var GameObject = require('./gameobject')
  , Vector     = require('./vector');

/**
 * Player has the following attributes (in addition to GameObject's)
 *  - name
 *  - movement
 */
var Player = module.exports = function(id, attributes) {
  GameObject.call(this, id, Player.TYPE, attributes);

  // Assert movement exists
  this.set('movement', _.clone(this.get('movement')) || { friction: 0.9, force: 0.3 });

  // Create acceleration information
  this.get('movement').acceleration = { x:0, y:0 };
};

Player.TYPE = 'Player';

GameObject.extend(Player);

Player.prototype.move = function(direction) {
  var movement = this.get('movement')
    , v        = new Vector(direction);

  // Normalize over movement force
  v = v.scalarMulti(movement.force / v.norm());

  // Set direction of burst (change existing burts)
  movement.acceleration.x = v.x;
  movement.acceleration.y = v.y;
};

Player.prototype.stop = function() {
  var movement = this.get('movement');
  movement.acceleration.x = 0;
  movement.acceleration.y = 0;
  this.emit('stopped', this);
};

Player.prototype._updateObject = function(position, velocity, duration, game) {
  var movement = this.get('movement')
    , before   = Math.pow(velocity.x, 2) + Math.pow(velocity.y, 2);

  // Adjust velocity using friction and acceleration
  velocity.x = (1 - movement.friction * duration) * velocity.x + duration * movement.acceleration.x;
  velocity.y = (1 - movement.friction * duration) * velocity.y + duration * movement.acceleration.y;

  // Prevent too small values for velocity
  if (Math.abs(velocity.x) < 0.01) velocity.x = 0;
  if (Math.abs(velocity.y) < 0.01) velocity.y = 0;

  // Invoke super method to update object
  GameObject.prototype._updateObject.apply(this, arguments);

  // Contain inside board by bouncing of walls
  var size  = this.getSize()
    , board = game.getBoard();

  if (position.x < board.left) {
    // Bounce on left wall
    velocity.x = -velocity.x;
    position.x = board.left;
  }
  if (position.x + size.width  > board.right) {
    // Bounce on right wall
    velocity.x = -velocity.x;
    position.x = board.right - size.width;
  }
  if (position.y < board.top) {
    // Bounce on top wall
    velocity.y = -velocity.y;
    position.y = board.top;
  }
  if (position.y + size.height > board.bottom) {
    // Bounce on bottom wall
    velocity.y = -velocity.y;
    position.y = board.bottom - size.height;
  }

  // Check if player stopped and emit
  if (before > 0 && velocity.x === 0 && velocity.y === 0) {
    this.emit('still', this);
  }
};
