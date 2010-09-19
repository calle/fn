var Client = require('./client'),
    net = require('net');

/**
 * The SocketClient object
 */
 
var SocketClient = module.exports = function(stream) {
  if (!(this instanceof SocketClient)) return new SocketClient(server);
  Client.call(this);
  
  this.stream = stream;
  
  // Setup stream
  stream.setEncoding('ascii');
  stream.on('data',  this.receivedData.bind(this));
  stream.on('close', this.clientClosedConnection.bind(this));
  stream.on('end',   this.connectionFullyClosed.bind(this));
  stream.on('error', this.connectionError.bind(this));

  this._trace("connected");
}
sys.inherits(SocketClient, Client);

/*
 * Client socket specific implementation
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

SocketServerClient.prototype._send = function(message) {
  this.stream.write(message + "\n");
  this.stream.flush();
}

// Also override _trace to display remote ip:port as well

SocketServerClient.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments),
      ip = this.stream.remoteAddress, 
      port = this.stream.remotePort;
  this.server._trace(["ServerClient[%s:%d]: " + args.shift(), ip, port].concat(args))
}

/*
 * Stream callback methods
 */

// Received data
ServerClient.prototype.receivedData = function(data) {
  // Append data to buffer
  this.buffer += data;
  
  // Split buffer at \n
  var parts = buffer.split(/\n/);

  // Invoke message handle for each part but the last
  while (parts.length > 1) this.handleMessage(parts.shift());

  // Parts now contains only the last part, make this the current buffer
  buffer = parts.shft();
}

// Client closed the connection
ServerClient.prototype.clientClosedConnection = function() {
  this._trace("remote side closed connection");
  this._logout();
  this.stream.end();
}
  
// Connection is fully closed
ServerClient.prototype.connectionFullyClosed = function(had_error) {
  this._trace("terminated");
  this.server.clientDisconnected(this);
}

ServerClient.prototype.connectionError = function(exception) {
  this._trace("error: %j", exception);
}