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
  stream.on('connect', function () {
    // Connected, add this client
    self.clients[id] = {
      stream: stream,
      messages: {},
      messageId: 0
    }
    self._send(id, 'login:' + name, function(response) {
      if (response === "error") {
        callbacks.login("error");
      } else {
        var parts = response.split(/,/, 4);
        callbacks.login(null, {
          x: parts[0],
          y: parts[1],
          dir: parts[2],
          size: parts[3],
          clients: parts[4] && parts[4] !== "" ? parts[4].split(/,/) : []
        });
      }
    });
  });

  stream.on('data', function (data) {
    console.log("Received data: " + data);
    var parts = data.split(/:/);
    var command = parts.shift();
    
    if (command === "response") {
      console.log("Response data: " + parts[1]);
      var messageId = parts.shift();
      var callback = self.clients[id].messages[messageId];
      if (callback && typeof callback === "function") {
        console.dir(parts);
        callback(parts.join(':'));
      }
      delete self.clients[id].messages[messageId];
    } else if (command === "update") {
      callbacks.update(parts[0]);
    }
  });

  stream.on('end', function () {
    delete self.clients[id];
    callbacks.end();
  });
};

Battlefield.prototype.logout = function(id, callback) {
  var self = this;
  this._send(id, "logout", function() {
    self.clients[id].stream.close();
    if (callback) {
      callback(null);
    }
  })
}

Battlefield.prototype.move = function(id, direction, callback) {
  this._send(id, "move:" + direction, function(response) {
    if (response === "error") {
      callback("error");
    } else {
      // Response is x,y,dir,size
      var message = response.split(/,/, 4);

      callback(null, {
        x: message[0],
        y: message[1],
        dir: message[2],
        size: message[3]
      });
    }
  })
}

Battlefield.prototype.shoot = function(id, position, callback) {
  this._send(id, "shoot:" + position.x + "," + position.y, function(response) {
    if (response === "error") {
      callback("error");
    } else {
      // Response is either "miss" or "kill,name" 
      var message = response.split(/,/);
      
      if (message.length === 1) {
        callback(null, 
        {
          status: "miss"
        });
      } else {
        callback(null, {
          status: "kill",
          target: message[1]
        });
      }
    }
  })
}

Battlefield.prototype.taunt = function(id, playerName, message) {
  console.log("Taunting " + playerName + " with " + message);
  this._send(id, "taunt:" + playerName + ":" + message, function() {});
}

Battlefield.prototype._send = function(id, message, callback) {
  var client = this.clients[id];
  console.log("Client: " + client);
  if (client) {
    var mid = client.messageId;
    client.messageId += 1;
    client.messages[mid] = callback;
    client.stream.write(mid + ':' + message);
  } else {
    callback("error");
  }
};