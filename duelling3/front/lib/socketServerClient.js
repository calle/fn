var ServerClient = require('./serverClient'),
    net = require('net');

/**
 * The SocketServerClient object
 */
 
var SocketServerClient = module.exports = function(server, stream) {
  if (!(this instanceof SocketServerClient)) return new SocketServerClient(server);
  ServerClient.call(this, server);
  
  this.stream = stream;
  
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
 * SocketServer implementation
 */

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
  command = command.replace(/(.)(.*)/, function(s,c,cs) { return c.toUpperCase() + cs.toLowerCase(); });

  // Require user is logged in
  if (this.loggedIn || command === 'Login') {
    // Invoke command
    this._trace('handle%s(%d, %s)', command, id, rest);
    this['handle' + command](id, rest);
  } else {
    this._reply(id, "error:Not logged in");
  }
}
