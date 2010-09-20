var net = require('net'),
    sys = require('sys'),
    Board = require('./board'),
    ServerClient = require('./serverClient');

/**
 * The server object
 */

var Server = module.exports = function(options) {
  options |= {};
  
  // Default values
  var boardSize = options.boardSize || 8,
      port      = options.port      || 3001,
      hostname  = options.hostname  || 'localhost';

  if (!options.trace) this._trace = function() {};

  var self = this;

  // The board
  this.board = new Board(boardSize);

  // Connected clients
  var connectedClients = [];

  // Logged in clients
  var clients = {};
  this.login  = Server.prototype.login.bind(this, clients);
  this.logout = Server.prototype.logout.bind(this, clients);
  this.shoot  = Server.prototype.shoot.bind(this, clients);
  this.taunt  = Server.prototype.taunt.bind(this, clients);
  
  // Create the server
  this.server = new net.Server();
  this.server.on('connection', function(stream) {
    var client = new ClientStreamInterface(new Client(self), stream);

    connectedClients.push(client);
    stream.on('end', function() {
      connectedClients = connectedClients.filter(function(exist) { return exist !== client; });
    });
  });
  // this.server.on('close',      this.teminated.bind(this));

  // Start listening for connections
  server.listen(port, hostname, function() {
    self._trace('listening on %s:%d', hostname, port);
  });
}

Server.prototype.login = function(clients, client, name) {
  if (clients[name]) {
    return {
      error: 'User already exists with name "' + name + '"'
    };
  } else {
    clients[name] = client;
    return {
      board: this.board,
      clientNames: Object.keys(clients)
    };
  }
}

Server.prototype.logout = function(clients, name) {
  delete clients[name];
}

Server.prototype.taunt = function(client, from, to, message) {
  if (client[to]) {
    client[to].taunted(from, message);
    return true;
  } else {
    return false;
  }
}

Server.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments);
  console.log.apply(console, ["Server: " + (args.shift() || '')].concat(args));
}

Server.prototype.shoot = function(clients, by, position) {
  var killed = [];
  Object.keys(clients).forEach(function(name) {
    var client = clients[name];
    if (client.occupy(position)) {
      client.killed(by, position);
      killed.push(name);
    }
  })
  return killed;
}