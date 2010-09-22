var Client = require('./client/client'),
    ServerProxySocket = require('./server/server_proxy_socket'),
    sys = require('sys');

var clientName = process.argv[2] || 'Calle';

var drawBoard = function(board, client) {
  var rows = [], i, row, j;

  for (i = 0; i < board.height; i++) {
    row = [];
    for (j = 0; j < board.width; j++) {
      row.push('•');
    }
    rows.push(row);
  }

  var ships = [client];
  ships.forEach(function(ship) {
    board.reverseWalk(ship.position, ship.direction, ship.size, function(x, y, axis) {
      rows[y][x] = (x === ship.position.x && y === ship.position.y) ? 'X' : (axis === 'x' ? '-' : '|');
    });
  });
  
  output(rows.reverse().map(function(row) { return row.join(' '); }).join('\n'));
  output('')
};

var callbacks = function(login) {
  return {
    login: login,
    update: function(message) {
      output('got update: ' + message);
    },
  }
};

var output = function() {
  console.log.apply(console, arguments)
  process.stdout.flush();
}

// Create server
var server = new ServerProxySocket('localhost', 9000);

// Create and register client
var client = new Client(server);

server.on('error', function(err) {
  output('connection error: ' + err)
});

server.on('closed', function() {
  output('client terminated');
});

var terminate = function() {
  client.logout(function() {
    output('terminate: logged out')
  });
}

// Listen for connection
server.on('connected', function () {

  client.login(clientName, function(err, clientState) {

    if (err) {
      output('Failed to login: %j', err)
      return terminate();
    }

    output('successfull login for %s: %j', clientName, clientState);

    output('Drawing board')
    drawBoard(client.board, client);

    setTimeout(function() {
      output('Moving client forward');
      client.move('forward', function(err, result) {
        if (!err) {
          // Update position
          drawBoard(client.board, client)
        }
        output('Closing down')
        terminate();
      });
    }, 2000)
  });
});
