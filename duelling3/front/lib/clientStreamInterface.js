var StringProtocol = require('./stringProtocol')
    net = require('net');

/**
 * The ClientStreamInterface exposes a client over a stream so external clients
 * can connect to it.
 * 
 */
var ClientStreamInterface = module.exports = function(client, stream) {
  if (!(this instanceof ClientStreamInterface)) return new ClientStreamInterface(client, stream);

  // Interfaces
  this.client = client;
  this.stream = stream;
  this.protocol = new StringProtocol();

  // State
  this.buffer = "";
  
  // Setup client
  client.on('taunted', this.taunted.bind(this));
  client.on('killed',  this.killed.bind(this));
  
  // Setup stream
  stream.setEncoding('ascii');
  stream.on('data',  this.receivedData.bind(this));
  stream.on('close', this.clientClosedConnection.bind(this));
  stream.on('end',   this.connectionFullyClosed.bind(this));
  stream.on('error', this.connectionError.bind(this));

  this._trace("connected");
};

/*
 * Stream methods
 */

ClientStreamInterface.prototype.receivedData = function(data) {
  // Append data to buffer
  this.buffer += data;
  
  // Split buffer at \n
  var parts = buffer.split(/\n/);

  // Invoke message handle for each part but the last
  while (parts.length > 1) this.handleMessage(parts.shift());

  // Parts now contains only the last part, make this the current buffer
  buffer = parts.shft();
}

ClientStreamInterface.prototype.send = function(data) {
  this.stream.write(data);
  this.stream.flush();
}

ClientStreamInterface.prototype.clientClosedConnection = function() {
  // Client closed the connection
  this._trace("remote side closed connection");
  this._logout();
  this.stream.end();
}
  
ClientStreamInterface.prototype.connectionFullyClosed = function(had_error) {
  // Connection is fully closed
  this._trace("terminated");
}

ClientStreamInterface.prototype.connectionError = function(exception) {
  // Error from connection
  this._trace("error: %j", exception);
}

/*
 * Client methods 
 */

ClientStreamInterface.prototype.taunted = function(data) {
  this.update('taunted', data);
}

ClientStreamInterface.prototype.killed = function(data) {
  this.update('killed', data);
}

/*
 * Internal methods
 */

ClientStreamInterface.prototype.handleMessage = function(message) {
  var self = this;

  this._trace("received message: %s", message);

  // Take apart message
  var parts   = message.split(/:/),
      id      = parts.shift(),
      command = parts.shift(),
      rest    = parts.join(':'),
      data;

  // Validate id
  if (id === undefined) {
    return this._trace("failed parsing id for message");    
  }
  
  // Make sure command is valid
  if (!/[a-zA-Z]+/.test(command)) {
    this._trace("invlid command in message");
    return this.reply(id, 'error', { message:'Invalid command' });
  }

  // Lowercase command
  command = command.toLowerCase();

  // Require user is logged in
  if (this.loggedIn || command === 'login') {
    
    // Parse data for command
    var data = this.protocol.unpackRequest(command, rest)
    
    // Prepare response callback
    var callback = function(err, result) {
      if (err) {
        self.reply(id, 'error', err);
      } else {
        self.reply(id, command, result);
      }
    }
    
    this._trace('[%d] - client.%s(%j)', id, command, data);

    // Invoke command on client
    switch (command) {
      case 'login':
        return this.client.login(data.name, callback);
      case 'logout':
        return this.client.logout(callback);
      case 'move':
        return this.client.move(data.direction, callback);
      case 'shoot':
        return this.client.shoot(data.position, callback);
      case 'taunt':
        return this.client.taunt(data.name, data.message, callback);
      default:
        return this.reply(id, 'error', { message:'Unknown command' });
    }
    
  } else {
    this.reply(id, 'error', { message:'Not logged in' });
  }
}

ClientStreamInterface.prototype.reply = function(id, type, data) {
  var message = this.protocol.packResponse(type, data);
  this.send("response:" + id + ":" + message + this.protocol.messageSeparator);
};

ClientStreamInterface.prototype.update = function(type, data) {
  var message = this.protocol.packUpdate(type, data);
  this.send("update:" + message + this.protocol.messageSeparator);
};

ClientStreamInterface.prototype._trace = function() {
  var args = Array.prototype.slice.apply(arguments),
      address = this.stream.remoteAddress, 
      port = this.stream.remotePort;

  console.log.apply(console, ["ClientStreamInterface[%s:%d]: " + args.shift(), address, port].concat(args));
};
