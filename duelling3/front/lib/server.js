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

  // Setup the server private variables
  with (this._setupConnectedClientsList()) {
    self.clientConnected = clientConnected;
    self.clientDisconnected = clientDisconnected;
  }
  with (this._setupClientsList()) {
    self.clientLogin  = login;
    self.clientLogout = logout;
    self.clientTaunt  = taunt;
    self.clientShoot  = shoot;
  }

  // Create the server
  this.server = new net.Server();
  this.server.on('connection', self.clientConnected.bind(this));
  // this.server.on('close',      this.teminated.bind(this));

  // Start listening to 3001
  server.listen(port, hostname, function() {
    self._trace('listening on %s:%d', hostname, port);
  });
}

Server.prototype._setupConnectedClientsList = function() {
  // All connected clients
  var _connectedClients = [];
  return {
    clientConnected: function(stream) {
      var client = new ServerClient(this, stream);
      _connectedClients.push(client);
    },
    clientDisconnected: function(client) {
      _connectedClients = _connectedClients.filter(function(existing) {
        return existing !== client;
      });
    }
  };
}

Server.prototype._setupClientsList = function() {
  // Clients currently logged in
  var _clients = {};

  // Internal methods for clients
  var clientsArray = function () { 
    var result = [];
    Object.keys(_clients).forEach(function(name) {
      result.push(_clients[name]);
    })
    return result;
  };

  return {
    login: function(client, name) {
      if (_clients[name]) {
        return {
          error: 'User already logged in as "' + name + '"'
        };
      } else {
        _clients[name] = client;
        return {
          board: this.board,
          clientNames: Object.keys(_clients)
        };
      }
    },
    logout: function(name) {
      delete _clients[name];
    },
    taunt: function(from, to, message) {
      if (_clients[to]) {
        _clients[to].taunted(from, message);
        return true;
      } else {
        return false;
      }
    },
    shoot: function(by, position) {
      var killed = [];
      Object.keys(_clients).forEach(function(name) {
        var client = _clients[name];
        if (client.isInside(position)) {
          client.killed(by, position);
          killed.push(name);
        }
      })
      return killed;
    }
  };
}

Server.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments);
  console.log.apply(console, ["Server: " + (args.shift() || '')].concat(args));
}
