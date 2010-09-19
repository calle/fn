var ServerClient = require('./serverClient'),
    StringProtocol = require('./stringProtocol')
    net = require('net');

/**
 * The SocketServerClient object
 */
 
var SocketServerClient = module.exports = function(server, stream) {
  if (!(this instanceof SocketServerClient)) return new SocketServerClient(server);
  ServerClient.call(this, server);
  
  this.stream = stream;
  this.protocol = new StringProtocol();
  
  // Setup stream
  stream.setEncoding('ascii');
  stream.on('data',  this.receivedData.bind(this));
  stream.on('close', this.clientClosedConnection.bind(this));
  stream.on('end',   this.connectionFullyClosed.bind(this));
  stream.on('error', this.connectionError.bind(this));

  this._trace("connected");
}
sys.inherits(SocketServerClient, ServerClient);

/*
 * ServerClient implementation
 */

ServerClient.prototype._reply = function(id, type, data) {
  // Serialize message
  var message = this.protocol['pack' + capitalize(type) + 'Reply'](data)
  this._send("response:" + id + ":" + message);
}

ServerClient.prototype._update = function(type, data) {
  var message = this.protocol['pack' + capitalize(type) + 'Update'](data)
  this._send("update:" + message);
}

// Also override _trace to display remote ip:port as well

SocketServerClient.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments),
      ip = this.stream.remoteAddress, 
      port = this.stream.remotePort;
  this.server._trace(["ServerClient[%s:%d]: " + args.shift(), ip, port].concat(args))
}

/*
 * Stream methods
 */

SocketServerClient.prototype._send = function(type, data) {
  this.stream.write(message + this.protocol.messageSeperator);
  this.stream.flush();
}

// Received data
SocketServerClient.prototype.receivedData = function(data) {
  // Append data to buffer
  this.buffer += data;
  
  // Split buffer at \n
  var parts = buffer.split(/\n/);

  // Invoke message handle for each part but the last
  while (parts.length > 1) this._handleMessage(parts.shift());

  // Parts now contains only the last part, make this the current buffer
  buffer = parts.shft();
}

// Client closed the connection
SocketServerClient.prototype.clientClosedConnection = function() {
  this._trace("remote side closed connection");
  this._logout();
  this.stream.end();
}
  
// Connection is fully closed
SocketServerClient.prototype.connectionFullyClosed = function(had_error) {
  this._trace("terminated");
  this.server.clientDisconnected(this);
}

// Error from connection
SocketServerClient.prototype.connectionError = function(exception) {
  this._trace("error: %j", exception);
}

/*
 * Message handler method
 */

SocketServerClient.prototype._handleMessage = function(message) {
  this._trace("received message: %s", message);

  // Take apart message
  var parts   = message.split(/:/),
      id      = parts.shift(),
      command = parts.shift(),
      rest    = parts.join(':');

  // Make sure command is valid
  if (!/[a-zA-Z]+/.test(command)) {
    return this._trace("cannot parse command");
  }

  // Capitalize command
  command = capitalize(command);

  // Require user is logged in
  if (this.loggedIn || command === 'Login') {
    // Parse data for command
    var data = this.protocol['unpack' + capitalize(command) + 'Request'](rest)
    // Invoke command
    this._trace('handle%s(%d, %s)', command, id, data);
    this['handle' + command](id, data);
  } else {
    this._reply(id, 'error', { message:'Not logged in' });
  }
}

/*
 * Utility methods
 */

var capitalize = function(string) {
  return string.replace(/(.)(.*)/, function(s,c,cs) { return c.toUpperCase() + cs.toLowerCase(); });
}