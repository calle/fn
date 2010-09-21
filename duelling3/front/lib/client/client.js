var events = require('events'),
    sys = require('sys'),
    trace = require('../utils/trace'),
    Board = require('../utils/board.js');

/**
 * The Client object, holder for one client connection.
 *  
 * Extends EventEmitter with the following events:
 *  - userLogin(name, position, direction) 
 *  - userLogout(name)
 *  - userMoved(name, position, direction)
 *  - userKilled(name, by, position)
 *  - killed(by, position)
 *  - taunted(by, message)
 *
 *  @param server - 
 *    the server instance having interface: 
 *    login(name, cb), logout(cb), shoot(pos, cb), taunt(name, msg, cb)
 *
 */
var Client = module.exports = function(server) {
  if (!(this instanceof Client)) return new Client(server);
  events.EventEmitter.call(this);

  // The server interface
  this.server = server;
  
  // The client state
  this.name = undefined;
  this.loggedIn = false;
  this.alive = false; // We become alive when we login
  this.board = undefined;
  this.size  = 0;
  this.state = {};
}
sys.inherits(Client, events.EventEmitter);

/*
 * External interface methods
 */

Client.prototype.login = function(name, callback) {
  var self = this;
  
  // Start by logging out user if already logged in
  self.logout(function() {

    // Make sure login name does not contain ','-characters
    name = name.replace(/,/g, '');

    self.server.login(self, name, function(err, response) {
      if (err) return callback(err);

      self._trace('successful login to server with name %s', name);
      
      // Setup internal state
      self.name = name;
      self.loggedIn = true;
      self.alive = true;
      self.board = new Board(response.board);
      self.size  = _rand(1,5);
      self.state = {
        x:   _rand(self.board.width),
        y:   _rand(self.board.height),
        dir: _rand_item(['north', 'south', 'east', 'west'])
      };

      // TODO: Ask server if position is valid
      
      // Invoke callback
      callback(null, {
        board:    self.board,
        position: self.state,
        size:     self.size,
        clients:  response.clientNames
      });
    });

  });
};

Client.prototype.logout = function(callback) {
  // Make sure we are logged in
  if (!this.loggedIn) {
    this._trace('logout: not logged in yet');
    // Logout when not logged in is always successful
    return callback(null, true);
  }

  var self = this;

  this._trace('logout: performing server logout');
  this.server.logout(this.name, function(err) {
    if (err) return callback(err);

    // Update internal state
    delete self.name;
    self.loggedIn = false;
    self.alive = false;

    callback(null, true);
  });
};

Client.prototype.move = function(direction, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback({ message:'Not logged in' });

  var self = this;

  this.server.move(this.name, this.state.dir, function(err, result) {
    if (err) return callback(err);
    self._trace('move response from server: %j', result);
    self.state.x   = result.position.x;
    self.state.y   = result.position.y;
    self.state.dir = result.direction;
    callback(null, result);
  });
};

Client.prototype.shoot = function(position, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback({ message:'Not logged in' });

  this._trace('shoot(%d, %d)', position.x, position.y);

  // Make sure x and y are inside board
  if (!this.board.inside(position)) {
    return callback({ message:'Cannot shoot outside board' });
  }

  this.server.shoot(this.name, position, function(err, result) {
    if (err) return callback(err);
    callback(null, { kills:(result || []) });
  });
};

Client.prototype.taunt = function(name, message, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback({ message:'Not logged in' });

  this._trace('taunt(%s, %s)', name, message);

  this.server.taunt(this.name, name, message, function(err, result) {
    if (err) return callback(err);
    if (!result) callback({ message:'No such user' });
    callback(null, true);
  });
};

/*
 * Server interface methods
 */

Client.prototype.userLogin = function(name, position, direction) {
  this._trace('userLogin(%s, %d,%d, %d)', name, position.x, position.y, direction);
  this.emit('userLogin', name, position, direction);
};

Client.prototype.userLogout = function(name) {
  this._trace('userLogout(%s)', name);
  this.emit('userLogout', name);    
};

Client.prototype.userMoved = function(name, position, direction) {
  this._trace('userMoved(%s, %d,%d, %d)', name, position.x, position.y, direction);
  this.emit('userMoved', name, position, direction);
};

Client.prototype.userKilled = function(name, by, position) {
  this._trace('userKilled(%s, %s, %d,%d)', name, by, position.x, position.y);
  this.emit('userKilled', name, by, position);
};

Client.prototype.killed = function(by, position) {
  this._trace('killed(%s, %d,%d)', by, position.x, position.y);
  if (this.alive) {
    this.alive = false;
    this.emit('killed', by, position);
  }
};

ClientServerState.prototype.taunted = function(by, message) {
  this._trace('taunted(%s, %s)', by, message);
  this.emit('taunted', by, message);
}

/*
 * Query methods
 */

Client.prototype.__defineGetter__('position', function() {
  return { x:this.state.x, y:this.state.y, direction: this.state.dir };
});

/*
 * Internal methods
 */

Client.prototype._trace = trace.prefix("Client: ");