var EndpointClient = require('./endpointClient'),
    StringProtocol = require('./stringProtocol')
    net = require('net');

/**
 * The SocketEndpointClient object
 */
 
var SocketEndpointClient = module.exports = function(stream, updateCallback) {
  if (!(this instanceof SocketEndpointClient)) return new SocketEndpointClient(stream, updateCallback);
  EndpointClient.call(this);
  
  this.stream = stream;
  this.protocol = new StringProtocol();
  this.updateCallback = updateCallback;
  
  // Setup stream
  stream.setEncoding('ascii');
  stream.on('data',  this.receivedData.bind(this));
  stream.on('close', this.clientClosedConnection.bind(this));
  stream.on('end',   this.connectionFullyClosed.bind(this));
  stream.on('error', this.connectionError.bind(this));

  this._trace("connected");
}
sys.inherits(SocketEndpointClient, EndpointClient);

/*
 * EndpointClient implementation
 */

SocketEndpointClient.prototype._request = function(type, data, callback) {
  var message = this.protocol['pack' + capitalize(type) + 'Request'](data)
  this._send(id + ":" + type + ":" + message);
}

SocketEndpointClient.prototype._reply = function(id, type, message) {
  this._trace("_reply: receive response to %d: %s", id, message);

  // Extract callback
  var callback = this.incoming.callbacks[id];
  delete this.incoming.callbacks[id];

  // Invoke if callback is valid
  if (callback && typeof callback === "function") {
    this._trace("_reply: invoking callback for %d", id);
    callback(message);
  } else {
    this._trace("_reply: cannot find callback for %d", id);
  }
}

SocketEndpointClient.prototype._update = function(message) {
  this._trace("_update: receive update: %s", message);
  if (this.updateCallback) this.updateCallback(message);
}

/*
 * Stream methods
 */

SocketEndpointClient.prototype._send = function(type, data) {
  this.stream.write(message + this.protocol.messageSeperator);
  this.stream.flush();
}

/*
 * Utility methods
 */

var capitalize = function(string) {
  return string.replace(/(.)(.*)/, function(s,c,cs) { return c.toUpperCase() + cs.toLowerCase(); });
}