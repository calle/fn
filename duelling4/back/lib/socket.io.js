/**
 * Socket IO code
 */
var io = require('socket.io'),
    logger = require('node-logger').logger('socket-io'),
    EventEmitter = require('events').EventEmitter;

module.exports = function(app) {

  // Setup plain socket
  var socket = io.listen(app, { log:logger.debug.bind(logger) }); 
  app.set('socket', socket);

  // Setup a socket server with only "authenticated" clients
  var server = new Server(socket);
  app.set('socket.server', server);

}

var Server = function(socket) {
  var self = this;

  // Invoke EventEmitter constructor
  EventEmitter.call(this);

  // Logged in clients
  this.clients = {};

  // Setup socket messages
  socket.on('connection', function(client) {
    logger.info('client %s connected', client.sessionId);

    // Always handle disconnect
    client.on('disconnect', function() {
      logger.info('client %s disconnected', client.sessionId)
      // Make sure we remove the client (even if not authenticated)
      self.remove(client.sessionId);
    })

    // Notify listeners of connection
    self.emit('connection', client.sessionId, client);
  });
};

Server.prototype.__proto__ = EventEmitter.prototype;

Server.prototype.add = function(id, client) {
  this.clients[id] = client;
  if (this.clients[id].added) this.clients[id].added(this);
};

Server.prototype.remove = function(id) {
  if (this.clients[id]) {
    if (this.clients[id].removed) this.clients[id].removed(this);
    delete this.clients[id];
  }
};

Server.prototype.others = function(client) {
  var clients = this.clients;
  return Object.keys(clients).
    map(function(key) { return clients[key]; }).
    filter(function(c) { return c !== client });
};

Server.prototype.broadcast = function(message, exclude) {
  this.others(exclude).forEach(function(client) {
    client.send(message);
  });
};