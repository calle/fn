var GameObject = require('./gameobject');

/**
 * Player has the following attributes (in addition to GameObject's)
 *  - name
 *  - speed
 */
var Player = module.exports = function(id, attributes) {
  GameObject.call(this, id, Player.TYPE, attributes);
};

Player.TYPE = 'player';

GameObject.extend(Player);

Player.prototype.move = function(direction) {
  var velocity = this.getVelocity()
    , speed    = this.get('speed');

  if (direction === 'up') {
    velocity.y = -speed;
  } else if (direction === 'down') {
    velocity.y = speed;
  }
};

Player.prototype.stop = function() {
  this.setVelocity({ x: 0, y: 0 });
};

Player.prototype._validatePosition = function(position, velocity, duration, game) {
  var geom  = this.getGeometry()
    , board = game.getBoard();

  // Contain inside board, just update position but keep velocity
  if (geom.left   < board.left)   position.x = board.left;
  if (geom.right  > board.right)  position.x = board.right - geom.width;
  if (geom.top    < board.top)    position.y = board.top;
  if (geom.bottom > board.bottom) position.y = board.bottom - geom.height;
};