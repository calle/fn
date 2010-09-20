var net = require('net'),
    Board = require('./board'),
    ServerClient = require('./serverClient');

/**
 * A normal Server
 * 
 */
var Server = module.exports = function(options) {
  options |= {};
  
  // Default values
  var boardSize = options.boardSize || 8;

  if (!options.trace) this._trace = function() {};

  var self = this;

  // The board
  this.board = new Board(boardSize);

  // Connections
  var connections = [];

  // Logged in clients
  var clients = {};
  this.login  = Server.prototype.login.bind(this, clients);
  this.logout = Server.prototype.logout.bind(this, clients);
  this.shoot  = Server.prototype.shoot.bind(this, clients);
  this.taunt  = Server.prototype.taunt.bind(this, clients);
  
  // Create the server
  this.server = new net.Server();
  this.server.on('connection', Server.prototype.clientConnected.bind(this, connections, clients));
}

Server.prototype.listen = function(hostname, port) {
  port = port || 3001;
  hostname = hostname || 'localhost';

  // Start listening for connections
  server.listen(port, hostname, function() {
    self._trace('listening on %s:%d', hostname, port);
  });
}

Server.prototype.clientConnected = function(connections, clients, stream) {
  // Create new client and connection
  var client = new Client(this);
  var connection = new ClientStreamConnection(client, stream);

  // Add to connected clients
  connections.push(connection);
  
  // Listen for stream close and remove client
  stream.on('end', function() {
    // Remove from logged in clients
    Object.keys(clients).forEach(function(name) {
      if (clients[name] === client) {
        delete clients[name];
      }
    });
    // Remove from connections
    connections = connections.filter(function(existing) { 
      return existing !== connection; 
    });
  });
}

StreamServer.prototype.register = function(client) {
  // Ignore registrations from clients
}

StreamServer.prototype.unregister = function(client) {
  // Ignore unregistrations from clients
}

Server.prototype.login = function(clients, client, name, callback) {
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

Server.prototype.logout = function(clients, name, callback) {
  delete clients[name];
  callback(null);
}

Server.prototype.shoot = function(clients, by, position, callback) {
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

Server.prototype.taunt = function(client, from, to, message, callback) {
  if (client[to]) {
    client[to].taunted(from, message);
    callback(null, true);
  } else {
    callback(null, false);
  }
}

Server.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments);
  console.log.apply(console, ["Server: " + (args.shift() || '')].concat(args));
}
