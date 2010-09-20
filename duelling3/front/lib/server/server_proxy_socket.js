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

  // Default values
  hostname = hostname || 'localhost';
  port = port || 3001;

  // Connect stream
  var stream = net.createConnection(port, hostname)
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