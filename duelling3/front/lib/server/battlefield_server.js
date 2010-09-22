var Board = require('../utils/board'),
    Client = require('../client/client'),
    trace = require('../utils/trace');

/**
 * The normal Battlefield server object
 *
 */
var BattlefieldServer = module.exports = function(options) {
  options |= {};
  
  // Default values
  var boardSize = options.boardSize || 8;

  // The board
  var board = new Board(boardSize);

  // Clients
  var clients  = [];
  
  // Bind methods to private variables
  this.login  = BattlefieldServer.prototype.login .bind(this, clients, board);
  this.logout = BattlefieldServer.prototype.logout.bind(this, clients);
  this.move   = BattlefieldServer.prototype.move  .bind(this, clients, board);
  this.shoot  = BattlefieldServer.prototype.shoot .bind(this, clients, board);
  this.taunt  = BattlefieldServer.prototype.taunt .bind(this, clients);
}

BattlefieldServer.prototype.login = function(clients, board, client, name, callback) {
  this._trace('login(%s)', name);

  if (clients[name]) {
    return callback({ message:'User already exists with name "' + name + '"' });
  } 

  // Generate unique key for this client
  var key = clients.length;

  var client = new ClientServerState(client);
  client.login(board, name);
  
  // Update clients array with information about client
  clients[key] = client;

  // Notify the other clients
  clients.forEach(function(c) { c.userLogin(client.name); });

  // Response with successful login
  callback(null, {
    key:          key, 
    board:        { width:board.width, height:board.height },
    position:     { x:client.position.x, y:client.position.y },
    direction:    client.direction,
    size:         client.size,
    otherClients: clients.map(function(c) { return c.name; }).filter(function(n) { return n !== name; })
  });
}

BattlefieldServer.prototype.logout = function(clients, key, callback) {
  this._trace('logout(%s)', key);

  if (!(key in clients)) { return callback({ message:'User not logged in' }); }
  var client = clients[key];
  
  // Remove from clients
  clients.splice(clients.indexOf(client), 1);

  // Notify the other clients
  clients.forEach(function(c) { c.userLogout(client.name); });

  callback(null);
}

BattlefieldServer.prototype.move = function(clients, board, key, direction, callback) {
  this._trace('move(%s, %s)', key, direction);

  if (!(key in clients)) { return callback({ message:'User not logged in' }); }
  var client = clients[key];

  // Step client
  client.move(board, direction);

  // Notify the other clients
  // clients.forEach(function(c) { if (c !== client) c.userMoved(client.name, client.position, client.direction); });

  // Invoke callback
  callback(null, { 
    position: { x:client.position.x, y:client.position.y }, 
    direction: client.direction 
  });
}
  
BattlefieldServer.prototype.shoot = function(clients, board, key, position, callback) {
  this._trace('shoot(%s, %d, %d)', key, position.x, position.y);

  if (!(key in clients)) { return callback({ message:'User not logged in' }); }
  var client = clients[key];

  var killed = [];
  clients.forEach(function(other) {
    if (other.occupies(board, position)) {
      other.killed(by, position);

      // Notify the other clients
      clients.forEach(function(c) { if (c !== client && c !== other) c.userKilled(other.name); });

      killed.push(other.name);
    }
  });
  callback(null, killed);
}

BattlefieldServer.prototype.taunt = function(client, key, to, message, callback) {
  this._trace('taunt(%s, %s, %s)', key, to, message);

  if (!(key in clients)) { return callback({ message:'User not logged in' }); }
  var client = clients[key];

  var found = false;
  clients.forEach(function(other) {
    if (other.name === to) {
      other.taunted(client.name, message);
      found = true;
    }
  })
  callback(null, found);
}

BattlefieldServer.prototype._trace = trace.prefix("BattlefieldServer: ");