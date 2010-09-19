var net = require('net');

/**
 * The Client object, holder for one client connection.
 *
 * Subclasses need to override the following methods:
 *  _login(name, callback)
 *  _shoot(position, callback)
 *  _taunt(name, message, callback)
 *  _logout(callback)
 *  _reply(id, type, data)
 *
 */
 
var Client = module.exports = function() {
  // The client state
  this.name = undefined;
  this.loggedIn = false;
  this.alive = false; // We become alive when we login
  this.board = undefined;
  this.size  = 0;
  this.state = {};
}

/*
 * Message handler methods
 */


Client.prototype.handleLogin = function(id, name) {
  // Start by logging out user if already logged in
  this.doLogout(function() {

    // Make sure login name does not contain ','-characters
    var name = name.replace(/,/g, '')

    this._login(name, function(response) {
      if (!response || response.error) {

        // Error logging in
        this._reply(id, 'error', { mesages: (response.error || 'Failed to login') });

      } else {

        // Setup internal state
        this.name = name;
        this.loggedIn = true;
        this.alive = true;
        this.board = response.board;
        this.size  = _rand(1,5);
        this.state = {
              x: _rand(this.board.width),
              y: _rand(this.board.height),
            dir: _rand_item(['north', 'south', 'east', 'west'])
        };

        this._reply(id, 'login', {
          board: this.board,
          position: this.state,
          size: this.size,
          clients: response.clientNames
        });
      }
    });
    
  });
}

Client.prototype.handleMove = function(id, direction) {
  var next = this.board.step(this.state, direction, 1);

  this.state.x   = next.x;
  this.state.y   = next.y;
  this.state.dir = dir;

  return this._reply(id, 'move', { position: this.state });

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
}

Client.prototype.handleShoot = function(id, position) {
  var parts = position.split(/,/),
      x = parseInt(parts.shift(), 10),
      y = parseInt(parts.shift(), 10),
      position = { x:x, y:y };

  this._trace('shoot[%d](%d, %d)', id, x, y)

  // Make sure x and y are inside board
  if (!board.inside(position)) {
    return this._reply(id, 'error', { message:'Cannot shoot outside board' });
  }

  this._shoot(position, function(result) {
    this._reply(id, 'shoot', { kills:result });
  });
}

Client.prototype.handleTaunt = function(id, taunt) {
  var parts = taunt.split(/:/),
      name = parts.shift(),
      message = parts.join(/:/);

  this._taunt(name, message, function(result) {
    if (result) {
      this._reply(id, 'taunt');
    } else {
      this._reply(id, 'error', 'No such user');
    }
  });
}

Client.prototype.handleLogout = function(id, name) {
  this.doLogout(function() {¨
    this._reply(id, 'logout');
  });
}

/*
 * Internal methods
 */

Client.prototype.doLogout = function(callback) {
 if (this.loggedIn) {
   this._logout(function() {
     delete this.name;
     this.loggedIn = false;
     this.alive = false;
     callback();
   })
 } else {
   callback();
 }
}

/**
 * Helper methods
 */

var _pos = function(state) {
  return state.x + "," + state.y + "," + state.dir;
}

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