var MessageStream = require('../utils/message_stream'),
    sys = require('sys'),
    events = require('events'),
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

  this.stream = new MessageStream(stream);
  this.protocol = this.stream.protocol;

  // State
  this.client = null;
  this.buffer = "";
  this.lastRequestId = 0;
  this.requests = {};
  
  // Setup stream events
  stream.setEncoding('ascii');
  stream.on('message', this.streamMessage.bind(this));
  stream.on('error',   this.streamError.bind(this));
  stream.on('closed',  this.streamClosed.bind(this));
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

ServerProxyStream.prototype.login = function(client, name, callback) {
  this.request('login', { name:name }, callback);
}

ServerProxyStream.prototype.logout = function(client, callback) {
  this.request('logout', {}, callback);
}

ServerProxyStream.prototype.move = function(client, direction, callback) {
  this.request('move',  { direction:direction }, callback);
}

ServerProxyStream.prototype.shoot = function(client, position, callback) {
  this.request('shoot', { position:position }, callback);
}

ServerProxyStream.prototype.taunt = function(client, name, message, callback) {
  this.request('taunt', { name:name, message:message }, callback);
}

/*
 * Stream events
 */

ServerProxyStream.prototype.streamMessage = function(message) {
  this._trace("streamMessage: %s", message);

  var parts = message.split(/:/),
      type = parts.shift(),
      rest = parts.join(':')

  if (type === "response") {
    this.streamResponse(rest);
  } else if (type === "update") {
    this.streamUpdate(rest);
  } else {
    this._trace('streamMessage: unknown message type: ' + command)
  }
}

ServerProxyStream.prototype.streamError = function(error) {
  this._trace('streamError: %e', error);
  this.emit('error', error);
}

ServerProxyStream.prototype.streamClosed = function() {
  this._trace('streamClosed');
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

  // Step request id and add type and callback
  var requestId = this.lastRequestId;
  this.lastRequestId += 1;
  this.requests[requestId] = {
      type: type,
      callback: callback
  };
  
  // Send request
  var result = this.stream.request(requestId, type, data);

  if (!result) {
    // Failed to send request, remove requestId and invoke callback
    delete this.requests[requestId];
    return callback({ message:'Failed to send request' });
  }
}

ServerProxyStream.prototype.streamResponse = function(message) {
  var parts = message.split(/:/),
      requestId = parts.shift(),
      rest = parts.join(':');

  this._trace("streamResponse: receive response %d: %s", requestId, rest);

  // Extract request for this response
  var request = this.requests[requestId];
  delete this.requests[requestId];

  if (request) {
    this._trace("streamResponse: invoking callback for response %d", requestId);
    
    // Handle errors
    if (parts[0] === 'error') {
      request.callback(this.protocol.unpackErrorResponse(parts.join(':')));
    } else {
      // Parse data for command
      var data = this.protocol.unpackResponse(request.type, rest)
      request.callback(null, data);
    }

  } else {
    this._trace("streamResponse: cannot find request for response %d", requestId);
  }
}

ServerProxyStream.prototype.streamUpdate = function(message) {
  this._trace("streamUpdate: received update: %s", message);

  // Make sure client is registered
  if (!this.client) {
    this._trace('streamUpdate: no client registered for updates yet, ignore this update');
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
      this._trace("streamUpdate: unknown update type %s", type);
  }
}

ServerProxyStream.prototype._trace = trace.prefix("ServerProxyStream: ");