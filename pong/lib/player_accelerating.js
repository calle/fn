var GameObject = require('./gameobject');

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
  var movement = this.get('movement');

  // // Check if we are still reloading and then adjust force
  // if (this.acceleration.reloading > 0) {
  //   force *= 1 - (this.acceleration.reloading / bursts.reload);
  // }

  var length = Math.sqrt(direction.x * direction.x + direction.y * direction.y);

  // Set direction of burst (change existing burts)
  movement.acceleration.x = direction.x * (movement.force / length);
  movement.acceleration.y = direction.y * (movement.force / length);

  // // Set duration and reloaded times
  // this.acceleration.duration  = bursts.duration;
  // this.acceleration.reloading = bursts.reload;

  // console.log('Player[%d] "%s" moving: %s', this.id, this.get('name'), JSON.stringify(movement.acceleration)); 
};

Player.prototype.stop = function() {
  var movement = this.get('movement');
  movement.acceleration.x = 0;
  movement.acceleration.y = 0;
  this.emit('stopped', this);
  // console.log('Player[%d] "%s" stopped accelerating: %s', this.id, this.get('name'), JSON.stringify(movement.acceleration)); 
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

  // // Update duration and reloading times
  // this.acceleration.duration  -= duration;
  // this.acceleration.reloading -= duration;
  // 
  // // Turn off acceleration after duration
  // if (this.acceleration.duration < 0) {
  //   this.acceleration.x = 0;
  //   this.acceleration.y = 0;
  // }

  // console.log('player: y = %s, vy = %s, ay = %s', 
  //   this.getPosition().y.toFixed(2), velocity.y.toFixed(2), this.acceleration.y.toFixed(2));

  // Invoke super method to update object
  GameObject.prototype._updateObject.apply(this, arguments);

  // Contain inside board, just update position but keep velocity
  var size  = this.getSize()
    , board = game.getBoard();

  if (position.x < board.left)                 position.x = board.left;
  if (position.x + size.width  > board.right)  position.x = board.right - size.width;
  if (position.y < board.top)                  position.y = board.top;
  if (position.y + size.height > board.bottom) position.y = board.bottom - size.height;

  // Check if player stopped and emit
  if (before > 0 && velocity.x === 0 && velocity.y === 0) {
    this.emit('still', this);
  }
};