var events = require('events'),
    sys = require('sys'),
    trace = require('../utils/trace'),
    Board = require('../utils/board.js');

/**
 * The Client object, holder for one client connection.
 *  
 * Extends EventEmitter with the following events:
 *  - userLogin(name) 
 *  - userLogout(name)
 *  - userKilled(name)
 *  - killed(by, position)
 *  - taunted(by, message)
 *
 *  @param server - 
 *    the server instance having interface: 
 *    login(client, name, cb), logout(key, cb), move(key, dir), shoot(key, pos, cb), taunt(key, name, msg, cb)
 *
 */
var Client = module.exports = function(server) {
  if (!(this instanceof Client)) return new Client(server);
  events.EventEmitter.call(this);

  // The server interface
  this.server = server;
  
  // The client state
  this.key = undefined;
  this.loggedIn = false;
  this.alive = false; // We become alive when we login
  this.name = undefined;
  this.board = undefined;
  this.position = undefined;
  this.direction = undefined;
  this.size  = 0;
  this.clients = [];
}
sys.inherits(Client, events.EventEmitter);

/*
 * External interface methods
 */

Client.prototype.login = function(name, callback) {
  var self = this;

  var _login = function() {
    // Make sure login name does not contain ','-characters
    name = name.replace(/,/g, '');

    self.server.login(self, name, function(err, response) {
      if (err) return callback(err);

      self._trace('successful login to server with name %s: %j', name, response);

      // Setup internal state
      self.key       = response.key;
      self.loggedIn  = true;
      self.alive     = true;
      self.name      = name;
      self.board     = new Board(response.board.width, response.board.height);
      self.position  = response.position;
      self.direction = response.direction;
      self.size      = response.size;
      self.clients   = response.clients;

      // Invoke callback
      callback(null, {
        board:     self.board,
        position:  self.position,
        direction: self.direction,
        size:      self.size,
        clients:   self.clients
      });
    });
  };
  
  if (this.loggedIn) {
    // Start by logging out user if already logged in
    self.logout(_login);
  } else {
    _login();
  }
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
  this.server.logout(this.key, function(err) {
    if (err) return callback(err);

    // Update internal state
    delete self.key;
    self.loggedIn = false;
    self.alive = false;

    callback(null, true);
  });
};

Client.prototype.move = function(direction, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback({ message:'Not logged in' });

  var self = this;

  this.server.move(this.key, direction, function(err, result) {
    if (err) return callback(err);
    self._trace('move response from server: %j', result);
    self.position  = result.position;
    self.direction = result.direction;
    callback(null, { 
      position:  self.position, 
      direction: self.direction 
    });
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

  var self = this;
  this.server.shoot(this.key, position, function(err, result) {
    self._trace('shoot: response is (%j, %j)', err, result);
    if (err) return callback(err);
    callback(null, { kills:(result || []) });
  });
};

Client.prototype.taunt = function(name, message, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback({ message:'Not logged in' });

  this._trace('taunt(%s, %s)', name, message);

  this.server.taunt(this.key, name, message, function(err, result) {
    if (err) return callback(err);
    if (!result) callback({ message:'No such user' });
    callback(null, true);
  });
};

/*
 * Server interface methods
 */

Client.prototype.userLogin = function(name) {
  this._trace('userLogin(%s)', name);
  this.clients.push(name);
  this.emit('userLogin', name);
};

Client.prototype.userLogout = function(name) {
  this._trace('userLogout(%s)', name);
  var index = this.clients.indexOf(name);
  if (index >= 0) this.clients.splice(index, 1);
  this.emit('userLogout', name);    
};

Client.prototype.userKilled = function(name) {
  this._trace('userKilled(%s)', name);
  this.emit('userKilled', name);
};

Client.prototype.killed = function(by, position) {
  this._trace('killed(%s, %d,%d)', by, position.x, position.y);
  if (this.alive) {
    this.alive = false;
    this.emit('killed', by, position);
  }
};

Client.prototype.taunted = function(by, message) {
  this._trace('taunted(%s, %s)', by, message);
  this.emit('taunted', by, message);
};

/*
 * Internal methods
 */

Client.prototype._trace = trace.prefix("Client: ");