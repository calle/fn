var StringProtocol = require('../protocol/stringProtocol'),
    events = require('events'),
    sys = require('sys'),
    trace = require('../utils/trace');

/**
 * A server proxy using a stream to forward messages to real server implementation.
 * 
 * Extends EventEmitter with the following events:
 *  - error(exception)
 *  - closed()
 *
 *  @param stream - 
 *    the stream the server is connected to
 * 
 */
var ServerProxyStream = module.exports = function(stream) {
  if (!(this instanceof ServerProxyStream)) return new ServerProxyStream(stream);
  events.EventEmitter.call(this);

  this.stream = stream;
  this.protocol = new StringProtocol();

  // State
  this.client = null;
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
sys.inherits(ServerProxyStream, events.EventEmitter);

/*
 * Server methods
 */

ServerProxyStream.prototype.register = function(client) {
  if (this.client) throw new Error('Can only have one client connected at the time to a ServerProxyStream');
  this.client = client;  
}

ServerProxyStream.prototype.unregister = function(client) {
  if (this.client === client) {
    this.client = null;
  }
}

ServerProxyStream.prototype.login = function(name, callback) {
  this.request('login', { name:name }, callback);
}

ServerProxyStream.prototype.logout = function(name, callback) {
  this.request('logout', {}, callback);
}

ServerProxyStream.prototype.shoot = function(by, position, callback) {
  this.request('shoot', { by:by, posisition:position }, callback);
}

ServerProxyStream.prototype.taunt = function(from, to, message, callback) {
  this.request('taunt', { from:from, to:to, message:message }, callback);
}

/*
 * Stream methods
 */

ServerProxyStream.prototype.receivedData = function(data) {
  // Append data to buffer
  this.buffer += data;
  
  // Split buffer at \n
  var parts = buffer.split(this.protocol.messageSeparator);

  // Invoke message handle for each part but the last
  while (parts.length > 1) this.handleMessage(parts.shift());

  // Parts now contains only the last part, make this the current buffer
  buffer = parts.shft();
}

ServerProxyStream.prototype.send = function(data) {
  this.stream.write(data);
  this.stream.flush();
}

ServerProxyStream.prototype.connectionError = function(exception) {
  // Error from connection
  this._trace("error: %j", exception);
  
  // Signal the error
  this.emit('error', exception);
}

ServerProxyStream.prototype.serverClosedConnection = function() {
  // Server closed the connection
  this._trace("remote side closed connection");
  
  // Terminate the stream
  this.stream.end();
}
  
ServerProxyStream.prototype.connectionFullyClosed = function(had_error) {
  // Connection is fully closed
  this._trace("terminated");
  
  // Signal that the connection is closed
  this.emit('closed');
}

/*
 * Internal methods
 */

ServerProxyStream.prototype.request = function(type, data, callback) {
  // Make sure client is registered
  if (!this.client) {
    return callback({ message:'Not registered' });
  }
  
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

ServerProxyStream.prototype.handleMessage = function(message) {
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

ServerProxyStream.prototype.handleResponse = function(message) {
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

ServerProxyStream.prototype.handleUpdate = function(message) {
  this._trace("handleUpdate: received update: %s", message);

  // Make sure client is registered
  if (!this.client) {
    this._trace('handleUpdate: no client registered for updates yet, ignore this update');
    return;
  }
  
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

ServerProxyStream.prototype._trace = trace.prefix(function() {
  return ["ServerProxyStream[%s:%d]: ", this.stream.remoteAddress, this.stream.remotePort]; 
});