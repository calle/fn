var net = require('net');

var Battlefield = module.exports = function(server, port) {
  this.server = server;
  this.port = port;
  this.clients = {};
};

Battlefield.prototype.login = function(id, name, callbacks) {

  // Koppla upp mot servern
  var stream = net.createConnection(this.port, this.server)
  stream.setEncoding('ascii')
  stream.setNoDelay(true)

  var self = this;
  
  // Handle connection, login client
  stream.on('connect', function () {
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
          x:       parts.shift(),
          y:       parts.shift(),
          dir:     parts.shift(),
          size:    parts.shift(),
          clients: parts.filter(function(part) { return part; })
        });
      }
    });
  });

  var handleMessage = function(message) {
    console.log("Battlefield.handleMessage[" + id + "]: " + message);
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
    console.log("Battlefield.data[" + id + "]: " + data);
    // Append data to buffer
    buffer += data;
    // Split in messages
    var parts = buffer.split(/\n/)
    // Handle all but last part as full message
    while (parts.length > 1) handleMessage(parts.shift());
    // If buffer ends in split-char handle as full message as well
    if (buffer.match(/\n$/)) handleMessage(parts.pop());
    // Set buffer to remaning parts
    buffer = parts.join("\n")
  });

  stream.on('end', function () {
    // Remove this client
    delete self.clients[id];
    // Invoke end callback
    callbacks.end();
  });
};

Battlefield.prototype.logout = function(id, callback) {
  var self = this;
  this._send(id, "logout", function(err) {
    if (err) { 
      if (callback) callback(err); 
    } else {
      var client = self.clients[id];
      // Close the stream after logout
      if (client && client.stream) {
        client.stream.close();
      }
      if (callback) callback(null);
    }
  });
};

Battlefield.prototype.move = function(id, direction, callback) {
  console.log("Battlefield.move[" + id + "]: direction = " + direction);
  this._send(id, "move:" + direction, function(err, response) {
    if (err || response === "error") {
      if (callback) callback(err || "error");
    } else {
      // Response is x,y,dir,size
      var message = response.split(/,/);
      if (callback) callback(null, {
        x:    message[0],
        y:    message[1],
        dir:  message[2],
        size: message[3]
      });
    }
  });
};

Battlefield.prototype.shoot = function(id, position, callback) {
  console.log("Battlefield.shoot[" + id + "]: x=" + position.x + ", y=" + position.y);
  this._send(id, "shoot:" + position.x + "," + position.y, function(err, response) {
    if (err || response === "error") {
      if (callback) callback(err || "error");
    } else {
      // Response is either "miss" or "kill,name" 
      var message = response.split(/,/);
      if (message[0] === "miss") {
        if (callback) callback(null, {
          status: "miss"
        });
      } else if (message[0] == "kill") {
        if (callback) callback(null, {
          status: "kill",
          target: message[1]
        });
      } else {
        if (callback) callback("Unknown response to shoot: " + message[0]);
      }
    }
  })
}

Battlefield.prototype.taunt = function(id, playerName, message) {
  console.log("Battlefield.taunt[" + id + "]: player " + playerName + " with " + message);
  this._send(id, "taunt:" + playerName + ":" + message, function(err, response) {
  });
}

Battlefield.prototype._send = function(id, message, callback) {
  console.log("Battlefield._send[" + id + "]: " + message);
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
    console.log("Battlefield._send[" + id + "]: sending message with id " + messageId);
    client.stream.write(messageId + ':' + message + "\n");
  } else {
    callback("error");
  }
};

Battlefield.prototype._receive = function(id, message) {
  console.log("Battlefield._receive[" + id + "]: " + message);
 
  var parts = message.split(/:/) 
  var messageId = parts.shift();
  console.log("Battlefield._receive[" + id + "]: receive response to message with id " + messageId);

  var client = this.clients[id];
  if (client && client.messageCallbacks) {
    // Extract callback
    var callback = client.messageCallbacks[messageId];
    delete client.messageCallbacks[messageId];

    // Invoke if callback is valid
    if (callback && typeof callback === "function") {
      console.log("Battlefield._receive[" + id + "]: invoking callback for message with id " + messageId + " with data: " + parts.join(":"));
      callback(null, parts.join(':'));
    } else {
      console.log("Battlefield._receive[" + id + "]: cannot find callback for message with id " + messageId);
    }
  } else {
    console.log("Battlefield._receive[" + id + "]: cannot find client");
  }
};
