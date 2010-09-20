var StreamServer = require('./streamServer'),
    net = require('net'),
    sys = require('sys');

/**
 * A SocketServer connecting to remove server using tcp-socket.
 * Extends StreamServer for actual implementation.
 */
var SocketServer = module.exports = function(hostname, port) {
  if (!(this instanceof SocketServer)) return new SocketServer(stream);

  // Connect stream
  var stream = net.createConnection(this.port, this.server)
  stream.setEncoding('ascii')

  // Invoke parent constructor
  StreamServer.call(self, stream);

  // Listen for connection
  var self = this;
  stream.on('connect', function () {
    // Set no delay on stream
    stream.setNoDelay(true)
    
    // Emit connected
    self.emit('connected');
  });
}
sys.inherits(SocketServer, StreamServer);