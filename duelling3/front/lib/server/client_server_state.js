var trace = require('../utils/trace');

/**
 * The ClientServerState is responsible for one clients state in the server
 * 
 */
var ClientServerState = module.exports = function(client) {
  if (!(this instanceof ClientServerState)) return new ClientServerState(client);

  this.target = client;
  
  // State
  this.name      = null;
  this.loggedIn  = false;
  this.alive     = false;
  this.position  = null;
  this.direction = null;
  this.size      = 0;

};

ClientServerState.prototype.login = function(board, name) {

  // Setup state
  this.name = name;
  this.loggedIn = true;
  this.alive    = true;
  
  // Random position, direction and size
  this.position  = { x:rand(board.width), y:rand(board.height) };
  this.direction = rand_item(['north', 'south', 'east', 'west']);
  this.size      = rand(1, 5);

};

ClientServerState.prototype.move = function(board, direction) {
  // Step using board
  var next = board.step(this.position, direction, 1);

  // Update position
  this.position = next;
  this.direction = direction;

  // TODO: Implement the following logic here for moves
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

ClientServerState.prototype.occupies = function(board, position) {
  var self = this;

  // Only check for occupation if we are logged in and alive
  if (!(this.loggedIn && this.alive)) return false;

  var end = { x:this.position.x, y:this.position.y },
      match = false;

  // Walk (using the board) over our ship and look for match
  board.reverseWalk(end, this.direction, this.size, function(x, y) {
    self._trace('occupies: looking for hit for shot %d,%d at %d,%d', position.x, position.y, x, y);
    if (position.x === x && position.y === y) {
      match = true;
      return Board.STOP;
    }
  })

  return match;
}

/*
 * Updates
 */

ClientServerState.prototype.userLogin = function(name) {
  this.target.userLogin(name);
};

ClientServerState.prototype.userLogout = function(name) {
  this.target.userLogout(name);    
};

ClientServerState.prototype.userKilled = function(name) {
  this.target.userKilled(name);
};

ClientServerState.prototype.killed = function(by, position) {
  if (this.alive) {
    this.alive = false;
    this.target.killed(by, position);    
  }
};

ClientServerState.prototype.taunted = function(by, message) {
  this.target.taunted(by, message);
};

ClientServerState.prototype._trace = trace.prefix("ClientServerState: ");

/**
 * Helper methods
 */

var rand = function(min, max) {
  if (!max) {
    max = min - 1;
    min = 0;
  }
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

var rand_item = function(items) {
  return items[_rand(items.length)];
}