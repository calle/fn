var events = require('events'),
    sys = require('sys'),
    trace = require('../utils/trace');

/**
 * The Client object, holder for one client connection.
 *  
 * Extends EventEmitter with the following events:
 *  - taunted { from, message }
 *  - killed { by, position }
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
 * Action methods
 */

Client.prototype.login = function(name, callback) {
  var self = this;
  
  // Start by logging out user if already logged in
  self.logout(function() {

    // Make sure login name does not contain ','-characters
    name = name.replace(/,/g, '')

    self.server.login(this, name, function(err, response) {
      if (err) return callback(err);

      // Setup internal state
      self.name = name;
      self.loggedIn = true;
      self.alive = true;
      self.board = response.board;
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
  var self = this;

  // Make sure we are logged in
  if (!this.loggedIn) {
    // Logout when not logged in is always successful
    return callback(null, true);
  }

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
  
  var next = this.board.step(this.state, direction, 1);

  // TODO: Ask server if move is valid
  
  this.state.x   = next.x;
  this.state.y   = next.y;
  this.state.dir = dir;

  callback(null, { position: this.state });

  // TODO: Implement functionality like the previous below
/*
  var now  = coords[client.state.dir],
      next = coords[direction];

  if (now.axis === next.axis) {
    // Same axis, move forward
    client.state[next.axis] += next.value;
    // Different direction, flip the ship around
    if (now.value !== next.value) {
      client.state[next.axis] += (next.value * (client.state.size - 1));
    }
  } else if (client.state.size > 2) {
    // Turn large ships
    var half_size = Math.floor((client.state.size - 1) / 2);
    client.state[now.axis]  += -now.value * half_size;
    client.state[next.axis] += next.value * half_size
  } else {
    // Just move ship
    client.state[next.axis] += next.value;
  }

  // Update direction
  client.state.dir = direction;

  // Normalize client x and y
  client.state.x = (client.state.x + board.width ) % board.width;
  client.state.y = (client.state.y + board.height) % board.height;
*/
};

Client.prototype.shoot = function(position, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback({ message:'Not logged in' });

  this._trace('shoot(%d, %d)', position.x, position.y);

  // Make sure x and y are inside board
  if (!board.inside(position)) {
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
 * Query methods
 */

Client.prototype.__defineGetter__('position', function() {
  return { x:this.state.x, y:this.state.y, direction: this.state.dir };
});

Client.prototype.occupies = function(position) {
  var self = this;

  // Only check for hits if we are logged in and alive
  if (!(this.loggedIn && this.alive)) return false;

  var end = { x:this.state.x, y:this.state.y },
      direction = this.state.dir,
      steps = this.size,
      match = false;

  // Walk (using the board) over our ship and look for match
  this.board.reverseWalk(end, direction, steps, function(x, y) {
    self._trace('isInside: looking for hit for shot %d,%d at %d,%d', position.x, position.y, x, y);
    if (position.x === x && position.y === y) {
      match = true;
      return Board.STOP;
    }
  })

  return match;
}

/*
 * Internal methods
 */

Client.prototype._trace = trace.prefix("Client: ");

/**
 * Helper methods
 */

var _rand = function(min, max) {
  if (!max) {
    max = min - 1;
    min = 0;
  }
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

var _rand_item = function(items) {
  return items[_rand(items.length)];
}