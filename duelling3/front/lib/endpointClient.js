var Client = require('./client'),
    net = require('net');

/**
 * The EndpointClient object
 *
 * Subclasses need to override the following methods:
 *  _request(type, data, callback)
 *  _reply(id, type, data)
 *
 */

var EndpointClient = module.exports = function() {
  if (!(this instanceof EndpointClient)) return new EndpointClient();
  Client.call(this);

  this.incoming = {
    callbacks = {},
    lastMessageId = 0
  }
  this.outgoing = {
    callbacks = {},
    lastMessageId = 0
  }
}
sys.inherits(EndpointClient, Client);

/*
 * External interface
 */

EndpointClient.prototype.login = function(name, callback) {
  this.doInvokeWithReply(function(id) {
    this.handleLogin(id, name)
  }, function(err, response) {
    if (callback) {
      if (response) {
        // Wrap board from response
        response.board = new Board(response.board.width, response.board.height);
      }
      callback(err, response);
    }
  });
};

EndpointClient.prototype.logout = function(callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback && callback("not logged in");

  this.doInvokeWithReply(function(id) {
    this.handleLogout(id)
  }, callback);
};

EndpointClient.prototype.move  = function(direction, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback && callback("not logged in");

  this.doInvokeWithReply(function(id) {
    this.handleMove(id, direction)
  }, callback);
};

EndpointClient.prototype.shoot = function(position, callback) {
  // Make sure we are logged in
  if (!this.loggedIn) return callback && callback("not logged in");

  this.doInvokeWithReply(function(id) {
    this.handleLogout(id)
  }, callback);
};

EndpointClient.prototype.taunt = function(playerName, message, callback) {
  this.doInvokeWithReply(function(id) {
    this.handleLogout(id)
  }, callback);
};


/*
 * Endpoint specific implementation
 */

EndpointClient.prototype._login = function(name, callback) {
  this._request('login', { name:name }, callback);
}

EndpointClient.prototype._shoot = function(position, callback) {
  this._request('shoot', { position:position }, callback);
}

EndpointClient.prototype._taunt = function(name, message, callback) {
  this._request('taunt', { name:name, message:message }, callback);
}

EndpointClient.prototype._logout = function(callback) {
  this._request('logout', {}, callback);
}

/*
 * Internal methods
 */

EndpointClient.prototype.doInvokeWithReply = function(idCallback, replyCallback) {
  var messageId = this.incoming.lastMessageId;
  this.incoming.lastMessageId += 1;
  this.incoming.callbacks[messageId] = replyCallback;
  
  idCallback(messageId);
}

EndpointClient.prototype.doSend = function(message, callback) {
 var messageId = this.outgoing.lastMessageId;
 this.outgoing.lastMessageId += 1;
 this.outgoing.callbacks[messageId] = callback;

 this._trace("doSend: sending message with id %d", messageId);
 this._send(messageId + ':' + message);
}

EndpointClient.prototype._receive = function(message) {
  var parts = message.split(/:/),
      messageId = parts.shift(),
      rest = parts.join(':');

  this._trace("_receive: receive response to %d: %s", messageId, rest);

  // Extract callback
  var callback = this.outgoing.callbacks[messageId];
  delete this.outgoing.callbacks[messageId];

  // Invoke if callback is valid
  if (callback && typeof callback === "function") {
    this._trace("_receive: invoking callback for %d", messageId);

    // Handle error callback
    if (parts[0] === 'error') {
      parts.shift();
      callback(parts.join(':'));
    } else {
      callback(null, rest);
    }
  } else {
    this._trace("_receive: invalid callback for %d", messageId);
  }
}
