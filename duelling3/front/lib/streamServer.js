var StringProtocol = require('./stringProtocol'),
    events = require('events'),
    sys = require('sys');

/**
 * A Server "implementation" using a stream to forward messages as strings over.
 *
 *    login(name, cb), logout(cb), shoot(pos, cb), taunt(name, msg, cb)
 *
 */
var StreamServer = module.exports = function(client, stream) {
  if (!(this instanceof StreamServer)) return new StreamServer(stream);
  events.EventEmitter.call(this);

  this.client = client;
  this.stream = stream;
  this.protocol = new StringProtocol();

  // State
  this.buffer = "";
  this.lastRequestId = 0;
  this.requests = {};
  
  // Setup stream
  stream.setEncoding('ascii');
  stream.on('data',  this.receivedData.bind(this));
  stream.on('close', this.serverClosedConnection.bind(this));
  stream.on('end',   this.connectionFullyClosed.bind(this));
  stream.on('error', this.connectionError.bind(this));
}
sys.inherits(StreamServer, events.EventEmitter);

/*
 * Server methods
 */

StreamServer.prototype.login = function(name, callback) {
  this.request('login', { name:name }, callback);
}

StreamServer.prototype.logout = function(name, callback) {
  this.request('logout', {}, callback);
}

StreamServer.prototype.shoot = function(by, position, callback) {
  this.request('shoot', { by:by, posisition:position }, callback);
}

StreamServer.prototype.taunt = function(from, to, message, callback) {
  this.request('taunt', { from:from, to:to, message:message }, callback);
}

/*
 * Stream methods
 */

StreamServer.prototype.receivedData = function(data) {
  // Append data to buffer
  this.buffer += data;
  
  // Split buffer at \n
  var parts = buffer.split(this.protocol.messageSeparator);

  // Invoke message handle for each part but the last
  while (parts.length > 1) this.handleMessage(parts.shift());

  // Parts now contains only the last part, make this the current buffer
  buffer = parts.shft();
}

StreamServer.prototype.send = function(data) {
  this.stream.write(data);
  this.stream.flush();
}

StreamServer.prototype.connectionError = function(exception) {
  // Error from connection
  this._trace("error: %j", exception);
  
  // Signal the error
  this.emit('error', exception);
}

StreamServer.prototype.serverClosedConnection = function() {
  // Server closed the connection
  this._trace("remote side closed connection");
  
  // Terminate the stream
  this.stream.end();
}
  
StreamServer.prototype.connectionFullyClosed = function(had_error) {
  // Connection is fully closed
  this._trace("terminated");
  
  // Signal that the connection is closed
  this.emit('closed');
}

/*
 * Internal methods
 */

StreamServer.prototype.request = function(type, data, callback) {
  // Make sure we are connected
  if (!this.stream.writable) {
    return callback({ message:'Not connected' });
  }

  // Serialize message
  var message = this.protocol.packRequest(type, data); 

  // Step request id and add type and callback
  var requestId = this.lastRequestId;
  this.lastRequestId += 1;
  this.requests[requestId] = {
      type: type,
      callback: callback
  };

  // Send request
  this._trace("send: sending message with id %d: %s", requestId, message);
  this.send(requestId + ':' + message);
}

StreamServer.prototype.handleMessage = function(message) {
  this._trace("handleMessage: %s", message);

  var parts = message.split(/:/),
      type = parts.shift(),
      rest = parts.join(':')

  if (type === "response") {
    this.handleResponse(rest);
  } else if (type === "update") {
    this.handleUpdate(rest);
  } else {
    this._trace('unknown message type: ' + command)
  }
}

StreamServer.prototype.handleResponse = function(message) {
  var parts = message.split(/:/),
      requestId = parts.shift(),
      rest = parts.join(':');

  this._trace("handleResponse: receive response %d: %s", requestId, rest);

  // Extract request for this response
  var request = this.requests[requestId];
  delete this.requests[requestId];

  if (request) {
    this._trace("_receive: invoking callback for response %d", requestId);
    
    // Handle errors
    if (parts[0] === 'error') {
      parts.shift();
      callback(this.protocol.unpackErrorResponse(parts.join(':')));
    } else {
      // Parse data for command
      var data = this.protocol.unpackResponse(request.type, rest)
      request.callback(null, data);
    }

  } else {
    this._trace("_receive: cannot find request for response %d", requestId);
  }
}

StreamServer.prototype.handleUpdate = function(message) {
  this._trace("handleUpdate: received update: %s", message);

  var parts = message.split(/:/),
      type = parts.shift(),
      rest = parts.join(':');

  var data = this.protocol.unpackUpdate(type, rest);

  switch (type) {
    case 'taunted':
      this.client.taunted(data.from, data.message);
      break;
    case 'killed':
      this.client.killed(data.by, data.position);
      break;
    default:
      this._trace("handleUpdate: unknown update type %s", type);
  }
}