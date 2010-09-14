
var Battlefield = function(server, port) {
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
    self._send(id, 'login:' + name)
  });

  stream.on('data', function (data) {
    var parts = data.split(/:/, 2);
    
    if (parts[0] === "response") {
      var message = parts[1].split(/:/, 2);
      var callback = messages[message[0]];
      if (callback && callback instanceof "function") {
        callback(message[1]);
      }
      delete messages[message[0]];
    } else if (parts[0] === "update") {
      callbacks.update(parts[1]);
    }
  });

  stream.on('end', function () {
    delete self.clients[id];
    callbacks.end();
  });
};

Battlefield.prototype.logout = function(client, callback) {
  var self = this;
  this._send(client, "logout", function() {
    self.clients[client].stream.close();
    if (callback) {
      callback(null);
    }
  })
}

Battlefield.prototype.move = function(client, direction, callback) {
  this._send(client, "move:" + direction, function(response) {
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

Battlefield.prototype.shoot = function(client, position, callback) {
  this._send(client, "move:" + direction, function(response) {
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

Battlefield.prototype.taunt = function(client, playerName, message) {
  this._send(client, "taunt:" + playerName + ":" + message, function() {});
}

Battlefield.prototype._send = function(client, message, callback) {
  client = this.clients[client];
  if (client) {
    var id = client.messageId;
    client.messageId += 1;
    client.messages[id] = callback;
    client.stream.write(id + ':' + message);
  } else {
    callback();
  }
};