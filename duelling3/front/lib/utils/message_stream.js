var StringProtocol = require('../protocol/string_protocol'),
    events = require('events'),
    sys = require('sys'),
    trace = require('../utils/trace');

/**
 * The MessageStream sends and receives message over a stream.
 * 
 */
var MessageStream = module.exports = function(stream) {
  if (!(this instanceof MessageStream)) return new MessageStream(stream);
  events.EventEmitter.call(this);

  var self = this;
  
  // Interfaces
  this.stream = stream;
  this.protocol = new StringProtocol();

  // State
  this.buffer = "";

  // Setup stream events
  stream.setEncoding('ascii');
  stream.on('data',  this.receivedData.bind(this));
  stream.on('error', this.connectionError.bind(this));
  stream.on('close', this.clientClosedConnection.bind(this));
  stream.on('end',   this.connectionFullyClosed.bind(this));

  this._trace("connected");
};
sys.inherits(MessageStream, events.EventEmitter);

/*
 * External interface methods
 */

MessageStream.prototype.send = function(data) {
  // Make sure we are connected
  if (!this.stream.writable) {
    return false;
  }
  
  this._trace('send: %s', data);
  this.stream.write(data);
  this.stream.flush();

  return true;
}

MessageStream.prototype.request = function(id, type, data) {
  // Serialize message
  this._trace("request: pack %s with data %j", type, data);
  var args = this.protocol.packRequest(type, data); 

  // Send request
  this._trace("request: sending %s request with id %d: %s", type, id, args);
  return this.send(id + ':' + type + ':' + args + this.protocol.messageSeparator);
};

MessageStream.prototype.reply = function(id, type, data) {
  // Serialize message
  this._trace('reply: pack %s with data %d', type, data);
  var args = this.protocol.packResponse(type, data);

  // Send reply
  this._trace('reply: sending %s reply with id %d: %s', type, id, args);
  return this.send("response:" + id + ":" + args + this.protocol.messageSeparator);
};

MessageStream.prototype.update = function(type, data) {
  // Serialize message
  this._trace('update: pack %s with data %d', type, data);
  var args = this.protocol.packUpdate(type, data);

  this._trace('update: sending %s update: %s', type, args);
  return this.send("update:" + type + ":" + args + this.protocol.messageSeparator);
};

/*
 * Stream event handlers
 */

MessageStream.prototype.receivedData = function(data) {
  // Append data to buffer
  this.buffer += data;
  
  // Split buffer at \n
  var parts = this.buffer.split(this.protocol.messageSeparator);

  // Emit message for each part but the last
  while (parts.length > 1) this.emit('message', parts.shift());

  // Parts now contains only the last part, make this the current buffer
  this.buffer = parts.shift();
}

MessageStream.prototype.connectionError = function(exception) {
  // Error from connection
  this._trace("error: %e", exception);
  
  // Signal the error
  this.emit('error', exception);
}

MessageStream.prototype.serverClosedConnection = function() {
  // Server closed the connection
  this._trace("remote side closed connection");
  
  // Terminate the stream
  this.stream.end();
}
  
MessageStream.prototype.connectionFullyClosed = function(had_error) {
  // Connection is fully closed
  this._trace("terminated");
  
  // Signal that the connection is closed
  this.emit('closed');
}

/*
 * Internal methods
 */

MessageStream.prototype._trace = trace.prefix(function() {
  if (this.stream.remoteAddress) {
    return ["MessageStream[%s:%d]: ", this.stream.remoteAddress, this.stream.remotePort]; 
  } else {
    return ["MessageStream: "];
  }
});