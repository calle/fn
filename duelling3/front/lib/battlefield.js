var net = require('net');

var Battlefield = module.exports = function(server, port, trace) {
  this.server = server;
  this.port = port;
  this.clients = {};
  if (!trace) this._trace = function() {}
};

Battlefield.prototype.login = function(id, name, callbacks) {

  // Koppla upp mot servern
  var stream = net.createConnection(this.port, this.server)
  stream.setEncoding('ascii')

  var self = this;
  
  // Handle connection, login client
  stream.on('connect', function () {
    stream.setNoDelay(true)

    // Connected, add this client
    self.clients[id] = {
      stream: stream
    };
    self._send(id, 'login:' + name, function(err, response) {
      if (err || response === "error") {
        callbacks.login("error");
      } else {
        var parts = response.split(/,/);
        callbacks.login(null, {
          board: {
            width:  parseInt(parts.shift(), 10),
            height: parseInt(parts.shift(), 10)
          },
          position: {
            x:         parseInt(parts.shift(), 10),
            y:         parseInt(parts.shift(), 10),
            direction: parts.shift(),
            size:      parseInt(parts.shift(), 10)
          },
          clients:  parts.filter(function(part) { return part; })
        });
      }
    });
  });

  var handleMessage = function(message) {
    self._trace("handleMessage", id, message);
    var parts = message.split(/:/);

    var command = parts.shift();
    if (command === "response") {
      self._receive(id, parts.join(":"));
    } else if (command === "update") {
      callbacks.update(parts.join(":"));
    }
  };

  var buffer = "";
  stream.on('data', function (data) {
    self._trace("data", id, "%d bytes", data.length);
    // Append data to buffer
    buffer += data;
    // Split in messages
    var parts = buffer.split(/\n/).filter(function(part) { return part !== ''; });
    // Handle all but last part as full message
    while (parts.length > 1) handleMessage(parts.shift());
    // If buffer ends in split-char handle as full message as well
    if (buffer.match(/\n$/)) handleMessage(parts.pop());
    // Set buffer to remaning parts
    buffer = parts.join("\n")
  });

  stream.on('error', function (err) {
    // Invoke error callback, then terminate
    if (callbacks.error) callbacks.error(err);
    stream.emit('end')
  });

  stream.on('end', function () {
    // Remove this client
    delete self.clients[id];
    // Invoke end callback
    callbacks.end();
  });
  
};

Battlefield.prototype.logout = function(id, callback) {
  this._trace("logout", id);
  var self = this;
  this._send(id, "logout", function(err) {
    if (err) { 
      if (callback) callback(err); 
    } else {
      var client = self.clients[id];
      // Close the stream after logout
      if (client && client.stream) {
        self._trace("logout", id, "closing stream for client");
        client.stream.end();
      }
      if (callback) callback(null);
    }
  });
};

Battlefield.prototype.move = function(id, direction, callback) {
  this._trace("move", id, "direction = %s", direction);
  this._send(id, "move:" + direction, function(err, response) {
    if (err || response === "error") {
      if (callback) callback(err || "error");
    } else {
      // Response is x,y,dir,size
      var message = response.split(/,/);
      if (callback) callback(null, {
        x:         parseInt(message.shift(), 10),
        y:         parseInt(message.shift(), 10),
        direction: message.shift(),
        size:      parseInt(message.shift(), 10)
      });
    }
  });
};

Battlefield.prototype.shoot = function(id, position, callback) {
  this._trace("shoot", id, "x=%d, y=%d", position.x, position.y);
  this._send(id, "shoot:" + position.x + "," + position.y, function(err, response) {
    if (err || response === "error") {
      if (callback) callback(err || "error");
    } else {
      // Response is either "miss" or "kill,name1,name2,..." 
      var parts = response.split(/,/),
          message = parts.shift();
      if (message === "miss") {
        if (callback) callback(null, {
          status: "miss"
        });
      } else if (message === "kill") {
        if (callback) callback(null, {
          status: "kill",
          targets: parts
        });
      } else {
        if (callback) callback("Unknown response to shoot: " + message);
      }
    }
  })
}

Battlefield.prototype.taunt = function(id, playerName, message, callback) {
  this._trace("taunt", id, "player %s with %s", playerName, message);
  this._send(id, "taunt:" + playerName + ":" + message, function(err, response) {
    if (callback) callback(err, response);
  });
}

Battlefield.prototype._send = function(id, message, callback) {
  this._trace("_send", id, message);
  var client = this.clients[id];
  if (client && client.stream) {
    if (!client.messageCallbacks) {
      // First message sent for this client
      client.messageCallbacks = {};
      client.lastMessageId = 0;
    }
    var messageId = client.lastMessageId;
    client.lastMessageId += 1;
    client.messageCallbacks[messageId] = callback;
    this._trace("_send", id, "sending message with id %d", messageId);
    client.stream.write(messageId + ':' + message + "\n");
  } else {
    callback("error");
  }
};

Battlefield.prototype._receive = function(id, message) {
  var parts = message.split(/:/) 
  var messageId = parts.shift();
  
  this._trace("_receive", id, "receive response to message with id %d: %s", messageId, message);

  var client = this.clients[id];
  if (client && client.messageCallbacks) {
    // Extract callback
    var callback = client.messageCallbacks[messageId];
    delete client.messageCallbacks[messageId];

    // Invoke if callback is valid
    if (callback && typeof callback === "function") {
      this._trace("_receive", id, "invoking callback for message with id %d with data: %s", messageId, parts.join(":"));
      callback(null, parts.join(':'));
    } else {
      this._trace("_receive", id, "cannot find callback for message with id %d", messageId);
    }
  } else {
    this._trace("_receive", id, "cannot find client");
  }
};


Battlefield.prototype._trace = function(method, clientId, message) {
  var args = Array.prototype.slice.call(arguments, 3);

  message = message ? ': ' + message : '';

  console.log.apply(console, ["Battlefield.%s[%d]" + message, method, clientId].concat(args));
};