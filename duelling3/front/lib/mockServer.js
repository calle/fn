var net = require('net');

// Clients currently logged in 
var clients = {};

var board = {
  width: 8,
  height: 8
};

var server = net.createServer(function (stream) {
  stream.setEncoding('ascii');

  stream.on('connect', function () {
    console.log("Client connected");
  });

  var client = {
    stream: stream,
    logged_in: false
  };

  var handleMessage = function(message) {
    console.log("Received message: " + message);
    
    var parts   = message.split(/:/),
        id      = parts.shift(),
        command = parts.shift();

    // Handle special case login
    if (command === "login") {
      // Logout user if already logged in
      if (client.logged_in) {
        delete clients[client.name];
        delete client.name;
        client.logged_in = false;
      }

      // Fix login name
      var name = parts.join(":").replace(",", "")
  
      if (clients[name]) {
        // Client with same name already exists
        return _reply(stream, id, "error");
      }
      
      // Update data and add to list of logged in clients
      client.name = name;
      client.logged_in = true;
      clients[name] = client;
    }

    // Require user is logged in 
    if (!client.logged_in) return _reply(stream, id, "error");

    // Invoke command
    server[command](client, id, parts.join(":"));
 
    // Handle special case logout
    if (command === "logout") {
      // Update data and remove from list of logged in clients
      delete clients[client.name];
      delete client.name;
      client.logged_in = false;
    }
  };

  var buffer = "";
  stream.on('data', function (data) {
    buffer += data;
    var parts = buffer.split(/\n/);
    while (parts.length > 1) handleMessage(parts.shift());
    if (buffer.match(/\n$/)) handleMessage(parts.pop());
    buffer = parts.join("\n");
  });

  stream.on('end', function () {
    console.log("Client died (logged out)");
    if (client.logged_in) {
      delete clients[client.name];
      delete client.name;
      client.logged_in = false;
    }
    stream.end();
  });

});

/**
 * Implement server commands 
 */

server.login = function(client, id, name) {
  client.state = {
    x: _rand(board.width),
    y: _rand(board.height),
    dir: _rand_item(['north', 'south', 'east', 'west']),
    size: _rand(1,5)
  }
  _reply(stream, id, _pos(client.state) + "," + Object.key(clients).join(","));
}

server.move = function(client, id, dir) {
  if (dir === "north") {
    client.state.y -= 1;
  } else if (dir === "south") {
    client.state.y += 1;
  } else if (dir === "east") {
    client.state.x += 1;
  } else if (dir === "west") {
    client.state.x -= 1;
  }

  // Normalize client x and y
  client.state.x = (client.stat.x + board.width) % board.width;
  client.state.y = (client.stat.y + board.height) % board.height;

  _reply(client.stream, id, _pos(client.state));
}

server.shoot = function(client, id, name) {
  _reply(client.stream, id, "miss");
}

server.taunt = function(client, id, name) {
  _reply(client.stream, id, "ok");
}

server.logout = function(client, id, name) {
  _reply(client.stream, id, "ok");
}

/**
 * Server helper methods
 */

var _pos = function(state) {
  return state.x + "," + state.y + "," + state.dir + "," + state.size;
}

var _reply = function(stream, id, message) {
  _send("response:" + id + ":" + message);
}
var _update = function(stream, message) {
  _send("update:" + message);
}
var _send = function(stream, message) {
  stream.write(message + "\n");
  stream.flush();
}

var _rand = function(min, max) {
  if (!max) {
    max = min - 1;
    min = 0;
  }
  return Math.floor(Math.random() * (max - min + 1)) + min;  
}
var _rand_item = function(items) {
  return items[_rand(items.length)];
}


// Start listening to 3001
server.listen(3001, 'localhost');
