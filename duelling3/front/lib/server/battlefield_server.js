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
  var registerdClients = [];
  var loggedInClients  = [];
  
  // Bind methods to private variables
  this.register   = BattlefieldServer.prototype.register  .bind(this, registerdClients);
  this.unregister = BattlefieldServer.prototype.unregister.bind(this, registerdClients, loggedInClients);
  this.login      = BattlefieldServer.prototype.login     .bind(this, loggedInClients, board);
  this.logout     = BattlefieldServer.prototype.logout    .bind(this, loggedInClients);
  this.move       = BattlefieldServer.prototype.move      .bind(this, loggedInClients, board);
  this.shoot      = BattlefieldServer.prototype.shoot     .bind(this, loggedInClients);
  this.taunt      = BattlefieldServer.prototype.taunt     .bind(this, loggedInClients);
}

BattlefieldServer.prototype.register = function(registeredClients, client) {
  this._trace('register(%s)', client);
  registeredClients.push(client);
}

BattlefieldServer.prototype.unregister = function(registeredClients, loggedInClients, client) {
  this._trace('unregister(%s)', client);

  // Remove from registered clients
  var index = registeredClients.indexOf(client);
  if (index >= 0) {
    registeredClients.splice(index, 1);
  }

  // Remove from logged in clients
  Object.keys(loggedInClients).forEach(function(name) {
    if (loggedInClients[name] === client) {
      delete clients[name];
    }
  });
}

BattlefieldServer.prototype.login = function(clients, board, client, name, callback) {
  this._trace('login(%s)', name);

  if (clients[name]) {
    callback({ message:'User already exists with name "' + name + '"' });
  } else {
    clients[name] = client;
    callback(null, {
      board: { width:board.width, height:board.height },
      clientNames: Object.keys(clients)
    });
  }
}

BattlefieldServer.prototype.logout = function(clients, name, callback) {
  this._trace('logout(%s)', name);
  delete clients[name];
  callback(null);
}

BattlefieldServer.prototype.move = function(clients, borad, name, direction, callback) {
  this._trace('move(%s)', direction);
  
  // TODO: Get state from clients map
  var state = { x:1, y:1, dir:'west' };
  
  var next = board.step(state, direction, 1);

  state.x   = next.x;
  state.y   = next.y;
  state.dir = direction;

  callback(null, { position:{ x:state.x, y:state.y}, direction:state.dir });
}
  
BattlefieldServer.prototype.shoot = function(clients, by, position, callback) {
  this._trace('shoot(%s, %d, %d)', by, position.x, position.y);

  var killed = [];
  Object.keys(clients).forEach(function(name) {
    var client = clients[name];
    if (client.occupies(position)) {
      client.killed(by, position);
      killed.push(name);
    }
  })
  callback(null, killed);
}

BattlefieldServer.prototype.taunt = function(client, from, to, message, callback) {
  this._trace('taunt(%s, %s, %s)', from, to, message);
  
  if (client[to]) {
    client[to].taunted(from, message);
    callback(null, true);
  } else {
    callback(null, false);
  }
}

BattlefieldServer.prototype._trace = trace.prefix("BattlefieldServer: ");