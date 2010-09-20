var ServerProxyStream = require('./server_proxy_stream'),
    net = require('net'),
    sys = require('sys');

/**
 * A server proxy connecting to remove server using tcp-socket.
 * Extends ServerProxyStream for actual implementation.
 * 
 */
var ServerProxySocket = module.exports = function(hostname, port) {
  if (!(this instanceof ServerProxySocket)) return new ServerProxySocket(stream);

  // Connect stream
  var stream = net.createConnection(this.port, this.server)
  stream.setEncoding('ascii')

  // Invoke parent constructor
  ServerProxyStream.call(self, stream);

  // Listen for connection
  var self = this;
  stream.on('connect', function () {
    // Set no delay on stream
    stream.setNoDelay(true)
    
    // Emit connected
    self.emit('connected');
  });
}
sys.inherits(ServerProxySocket, ServerProxyStream);