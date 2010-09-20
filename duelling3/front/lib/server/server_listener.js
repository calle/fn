var net = require('net');

var ServerListener = module.exports = function(server) {
  // The server we use
  this.server = server;

  // Connections
  var connections = [];

  // Create the server
  this.tcpServer = new net.Server();
  this.tcpServer.on('connection', ServerListener.prototype.clientConnected.bind(this, connections));
}

ServerListener.prototype.listen = function(hostname, port) {
  port = port || 3001;
  hostname = hostname || 'localhost';

  // Start listening for connections
  server.listen(port, hostname, function() {
    self._trace('listening on %s:%d', hostname, port);
});

  
ServerListener.prototype.clientConnected = function(connections, stream) {
  // Create new client
  var client = new ClientStreamConnection(new Client(this.server), stream);

  // Add to connected clients
  connections.push(clients);
  
  // Register client on server
  this.server.register(client);
  
  // Listen for stream close and remove client
  stream.on('end', function() {
    // Unregister client from server
    this.server.unregister(client);

    // Remove from connections
    var index = connections.indexOf(client);
    if (index >= 0) {
      connections.splice(index, 1);
    }
  });
}

ServerListener.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments);
  console.log.apply(console, ["ServerListener: " + (args.shift() || '')].concat(args));
}
