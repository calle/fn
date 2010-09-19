var Client = require('./client'),
    net = require('net');

/**
 * The ServerClient object, holder for one client connection
 *
 * Subclasses need to override the following methods:
 *  _send(message)
 *
 */

var ServerClient = module.exports = function(server) {
  if (!(this instanceof ServerClient)) return new ServerClient(server);
  Client.call(this);

  this.server = server;
}
sys.inherits(ServerClient, Client);

/*
 * Server specific implementation
 */

ServerClient.prototype._login = function(name, callback) {
  // Login to server
  callback(this.server.clientLogin(this, name));
}

ServerClient.prototype._shoot = function(position, callback) {
  // Ask server for possible kills
  callback(this.server.clientShoot(this.name, position));
}

ServerClient.prototype._taunt = function(name, message, callback) {
  // Send taunt to server
  callback(this.server.clientTaunt(this.name, name, message));
}

ServerClient.prototype._logout = function(callback) {
  // Logout from server
  this.server.clientLogout(this.name);
  callback();
}

// Also override _trace to use server _trace instead

ServerClient.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments);
  this.server._trace(["ServerClient: " + args.shift(), ip, port].concat(args))
}
