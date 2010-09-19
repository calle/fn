var net = require('net');

/**
 * The Client object, holder for one client connection.
 *
 * Subclasses need to override the following methods:
 *  _login(name, callback)
 *  _shoot(position, callback)
 *  _taunt(name, message, callback)
 *  _logout(callback)
 *  _send(message)
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
 * External interface
 */

Client.prototype.taunted = function(from, message) {
  this._update('taunted:' + from + ':' + message)
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
    this._update('killed:' + by + ':' + position.x + "," + position.y)
  }
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
        this._reply(id, "error:" + (response.error || 'Failed to login'));

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

        var response = ""
        // Append board information
        response += this.board.width + "," + this.board.height + ","
        // Append current position and size
        response += _pos(state) + "," + this.size + ","
        // Append names of the other player
        response += response.clientNames.join(",")

        this._reply(id, response);

      }
    });
    
  });
}

Client.prototype.handleMove = function(id, direction) {
  var next = this.board.step(this.state, direction, 1);

  this.state.x   = next.x;
  this.state.y   = next.y;
  this.state.dir = dir;

  return this._reply(id, _pos(this.state));

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
    return this._reply("error:Cannot shoot outside board");
  }

  this._shoot(position, function(result) {
    if (result.length > 0) {
      this._reply(id, "kill," + result.join(","));
    } else {
      this._reply(id, "miss");
    }
  });
}

Client.prototype.handleTaunt = function(id, taunt) {
  var parts = taunt.split(/:/),
      name = parts.shift(),
      message = parts.join(/:/);

  this._taunt(name, message, function(result) {
    if (result) {
      this._reply(id, "ok");
    } else {
      this._reply(id, "error:No such user")
    }
  });
}

Client.prototype.handleLogout = function(id, name) {
  this.doLogout(function() {¨
    this._reply(id, "ok");
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

Client.prototype._reply = function(id, message) {
  this._send("response:" + id + ":" + message);
}

Client.prototype._update = function(message) {
  this._send("update:" + message);
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