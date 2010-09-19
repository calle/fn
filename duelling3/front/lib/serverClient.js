var Client = require('./client'),
    net = require('net');

/**
 * The ServerClient object, holder for one client connection
 *
 * Subclasses need to override the following methods:
 *  _reply(id, type, data)
 *  _update(type, data)
 *
 */

var ServerClient = module.exports = function(server) {
  if (!(this instanceof ServerClient)) return new ServerClient(server);
  Client.call(this);

  this.server = server;
}
sys.inherits(ServerClient, Client);

/*
 * Server interface
 */

Client.prototype.taunted = function(from, message) {
  this._update('taunted', { from:from, message:message })
}

Client.prototype.isInside = function(position) {
  var self = this;
  
  // Only check for hits if we are logged in and alive
  if (!(this.loggedIn && this.alive)) return false;
  
  var end = { x:this.state.x, y:this.state.y },
      direction = this.state.dir,
      steps = this.size,
      hit = false;

  // Walk (using the board) over our ship and look for hit
  this.board.reverseWalk(end, direction, steps, function(x, y) {
    self._trace('isInside: looking for hit for shot %d,%d at %d,%d', position.x, position.y, x, y);
    if (position.x === x && position.y === y) {
      hit = true;
      return Board.STOP;
    }
  })

  return hit;
}

Client.prototype.killed = function(by, position) {
  if (this.alive) {
    this.alive = false;
    this._update('killed', { by:by, position:position })
  }
}

/*
 * Server specific implementation
 */

ServerClient.prototype._login = function(name, callback) {
  // Login to server
  callback(this.server.clientLogin(this, name));
}

ServerClient.prototype._shoot = function(position, callback) {
  // Ask server for possible kills
  callback(this.server.clientShoot(this.name, position));
}

ServerClient.prototype._taunt = function(name, message, callback) {
  // Send taunt to server
  callback(this.server.clientTaunt(this.name, name, message));
}

ServerClient.prototype._logout = function(callback) {
  // Logout from server
  this.server.clientLogout(this.name);
  callback();
}

// Also override _trace to use server _trace instead

ServerClient.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments);
  this.server._trace(["ServerClient: " + args.shift(), ip, port].concat(args))
}
