var MessageStream = require('../utils/message_stream'),
    sys = require('sys'),
    events = require('events'),
    trace = require('../utils/trace');

/**
 * The ClientProxyStream connects a stream to a client.
 * 
 */
var ClientProxyStream = module.exports = function(client, stream) {
  if (!(this instanceof ClientProxyStream)) return new ClientProxyStream(client, stream);
  events.EventEmitter.call(this);

  var self = this;
  
  // Interfaces
  this.client = client;
  this.stream = new MessageStream(stream);
  this.protocol = this.stream.protocol;

  // Setup client events
  client.on('userLogin',  this.userLogin.bind(this));
  client.on('userLogout', this.userLogout.bind(this));
  client.on('userKilled', this.userKilled.bind(this));
  client.on('taunted',    this.taunted.bind(this));
  client.on('killed',     this.killed.bind(this));

  // Setup message stream events
  this.stream.on('message', this.streamMessage.bind(this));
  this.stream.on('error',   this.streamError.bind(this));
  this.stream.on('closed',  this.streamClosed.bind(this));

  this._trace("setup complete");
};
sys.inherits(ClientProxyStream, events.EventEmitter);

/*
 * Client events
 */

ClientProxyStream.prototype.userLogin = function(name) {
  this.stream.update('userLogin', { name:name }); 
};

ClientProxyStream.prototype.userLogout = function(name) { 
  this.stream.update('userLogout', { name:name }); 
};

ClientProxyStream.prototype.userKilled = function(name) { 
  this.stream.update('userKilled', { name:name }); 
};

ClientProxyStream.prototype.taunted = function(by, message) { 
  this.stream.update('taunted', { by:by, message:message }); 
};

ClientProxyStream.prototype.killed = function(by, position) { 
  this.stream.update('taunted', { by:by, position:position }); 
};

/*
 * Stream events
 */

ClientProxyStream.prototype.streamMessage = function(message) {
  var self = this;

  this._trace("streamMessage: %s", message);

  // Take apart message
  var parts   = message.split(/:/),
      id      = parts.shift(),
      command = parts.shift(),
      rest    = parts.join(':'),
      data;

  // Validate id
  if (id === undefined) {
    return this._trace("streamMessage: failed parsing id for message");    
  }
  
  // Make sure command is valid
  if (!/[a-zA-Z]+/.test(command)) {
    this._trace("streamMessage: invlid command in message");
    return this.stream.reply(id, 'error', { message:'Invalid command' });
  }

  // Lowercase command
  command = command.toLowerCase();

  // Parse data for command
  var data = this.protocol.unpackRequest(command, rest)
  
  // Prepare response callback
  var callback = function(err, result) {
    self._trace('streamMessage: received callback(%j, %j)', err, result);
    if (err) {
    } else {
      self.stream.reply(id, command, result);
    }
  }
  
  this._trace('streamMessage: [%d] - client.%s(%j)', id, command, data);

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
      return this.stream.reply(id, 'error', { message:'Unknown command' });
  }
}

ClientProxyStream.prototype.streamError = function(error) {
  this._trace('streamError: %e', error);
  this.emit('error', error);
}

ClientProxyStream.prototype.streamClosed = function() {
  this._trace('streamClosed');
  // Logout client
  this.client.logout(function() {});
  this.emit('closed');
}

/*
 * Internal methods
 */

ClientProxyStream.prototype._trace = trace.prefix("ClientProxyStream: ");