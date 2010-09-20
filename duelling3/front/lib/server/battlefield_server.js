var Board = require('../board'),
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

  if (!options.trace) this._trace = function() {};

  var self = this;

  // The board
  this.board = new Board(boardSize);

  // Clients
  var registerdClients = [];
  var loggedInClients = {};
  
  // Bind methods to private variables
  this.register   = BattlefieldServer.prototype.login.bind(this, registerdClients);
  this.unregister = BattlefieldServer.prototype.login.bind(this, registerdClients, loggedInClients);
  this.login      = BattlefieldServer.prototype.login.bind(this,  loggedInClients);
  this.logout     = BattlefieldServer.prototype.logout.bind(this, loggedInClients);
  this.shoot      = BattlefieldServer.prototype.shoot.bind(this,  loggedInClients);
  this.taunt      = BattlefieldServer.prototype.taunt.bind(this,  loggedInClients);
}

BattlefieldServer.prototype.register = function(registeredClients, client) {
  registeredClients.push(client);
}

BattlefieldServer.prototype.unregister = function(registeredClients, loggedInClients, client) {
  // Remove from registered clients
  var index = registeredClients.indexOf(client);
  if (index >= 0) {
    registeredClients.splice(index, 1);
  }

  // Remove from logged in clients
  Object.keys(clients).forEach(function(name) {
    if (clients[name] === client) {
      delete clients[name];
    }
  });
}

BattlefieldServer.prototype.login = function(clients, client, name, callback) {
  if (clients[name]) {
    callback({ message:'User already exists with name "' + name + '"' });
  } else {
    clients[name] = client;
    callback(null, {
      board: this.board,
      clientNames: Object.keys(clients)
    });
  }
}

BattlefieldServer.prototype.logout = function(clients, name, callback) {
  delete clients[name];
  callback(null);
}

BattlefieldServer.prototype.shoot = function(clients, by, position, callback) {
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
  if (client[to]) {
    client[to].taunted(from, message);
    callback(null, true);
  } else {
    callback(null, false);
  }
}

BattlefieldServer.prototype._trace = trace.prefix("BattlefieldServer: ");