var GameObject = require('./gameobject')
  , Vector     = require('./vector');

/**
 * Player has the following attributes (in addition to GameObject's)
 *  - name
 *  - speed
 */
var Player = module.exports = function(id, attributes) {
  GameObject.call(this, id, Player.TYPE, attributes);
};

Player.TYPE = 'Player';

GameObject.extend(Player);

Player.prototype.move = function(direction) {
  // console.log('Player["%s"].move(%s)', this.get('name'), JSON.stringify(direction));

  var v = new Vector(direction);

  // Normalize over speed
  this.setVelocity(v.scalarMulti(this.get('speed') / v.norm()));

  // console.log('Player["%s"] moved: %s', this.get('name'), JSON.stringify(this.getVelocity())); 
};

Player.prototype.stop = function() {
  // console.log('Player["%s"].stop()', this.get('name'));
  this.setVelocity({ x: 0, y: 0 });
  this.emit('stopped', this, this.getPosition());
};

Player.prototype._updateObject = function(position, velocity, duration, game) {
  // Invoke super method to update object
  GameObject.prototype._updateObject.apply(this, arguments);

  // Contain inside board, just update position but keep velocity
  var size  = this.getSize()
    , board = game.getBoard();

  if (position.x < board.left)                 position.x = board.left;
  if (position.x + size.width  > board.right)  position.x = board.right - size.width;
  if (position.y < board.top)                  position.y = board.top;
  if (position.y + size.height > board.bottom) position.y = board.bottom - size.height;
};