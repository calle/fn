var net = require('net'),
    sys = require('sys');

// Clients currently logged in
var clients = {};

var board = {
  width:  16,
  height: 16
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

    if (!command) {
      console.log("Invalid message: " + message);
      return;
    }

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
        return _reply(stream, id, "error:Name already logged in");
      }

      // Update data and add to list of logged in clients
      client.name = name;
      client.logged_in = true;
      clients[name] = client;
    }

    // Require user is logged in
    if (!client.logged_in) return _reply(stream, id, "error: Must login first");

    // Invoke command
    console.log('invoke server.%s(%s, %d, %s)', command, client.name, id, parts.join(":"));
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
    var parts = buffer.split(/\n/).filter(function(part) { return part !== ''; });
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
  _reply(client.stream, id, board.width + "," + board.height + "," + _pos(client.state) + "," + Object.keys(clients).join(","));
}

var coords = {
  north: { axis: 'y', value:  1 },
  south: { axis: 'y', value: -1 },
  east:  { axis: 'x', value:  1 },
  west:  { axis: 'x', value: -1 },
}
server.move = function(client, id, dir) {
  var now  = coords[client.state.dir],
      next = coords[dir];

  if (now.axis === next.axis) {
    // Same axis, move forward
    client.state[next.axis] += next.value;
    // Different direction, flip the ship around
    if (now.value !== next.value) {
      client.state[next.axis] += (next.value * (client.state.size - 1));
    }
  } else if (client.state.size > 2) {
    // Turn large ships
    var half_size = Math.floor((client.state.size - 1) / 2);
    client.state[now.axis]  += -now.value * half_size;
    client.state[next.axis] += next.value * half_size
  } else {
    // Just move ship
    client.state[next.axis] += next.value;
  }

  // Update direction
  client.state.dir = dir;

  // Normalize client x and y
  client.state.x = (client.state.x + board.width ) % board.width;
  client.state.y = (client.state.y + board.height) % board.height;

  _reply(client.stream, id, _pos(client.state));
}

var _inside = function(state, x, y) {
  var coord = coords[state.dir],
      ship  = { x:state.x, y:state.y },
      board_size = board[coord.axis === 'x' ? 'width' : 'height'];

  // Start from tail of ship and to in direction to front
  ship[coord.axis] -= coord.value * state.size

  for (var i=0; i < state.size; i++) {
    ship[coord.axis] = (ship[coord.axis] + coord.value + board_size) % board_size;
    console.log('server._inside: looking for hit to %d,%d at %d,%d', x, y, ship.x, ship.y);
    if (ship.x === x && ship.y === y) return true;
  }
  return false;
}

server.shoot = function(client, id, pos) {
  var parts = pos.split(/,/),
      x = parseInt(parts.shift(), 10),
      y = parseInt(parts.shift(), 10);

  console.log('server.shoot(%d, %d, %d)', id, x, y)

  // Walk clients and find the ones hit
  var targets = [];
  Object.keys(clients).forEach(function(name) {
    var other = clients[name];
    if (other === client) return; // Cannot shoot yourself
    if (_inside(other.state, x, y)) {
      targets.push(other);
    }
  });

  // Look for targets
  if (targets.length > 0) {
    _reply(client.stream, id, "kill," + targets.map(function(c) {  return c.name; }).join(","));
  } else {
    _reply(client.stream, id, "miss");
  }
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
  _send(stream, "response:" + id + ":" + message);
}
var _update = function(stream, message) {
  _send(stream, "update:" + message);
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
