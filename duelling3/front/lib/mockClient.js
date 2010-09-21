var Client = require('./client/client'),
    ServerProxySocket = require('./server/server_proxy_socket'),
    stdio = process.binding('stdio'),
    sys = require('sys');

var clientName = process.argv[2] || 'Calle';

var stdin = process.openStdin();

var drawBoard = function(state) {
  var board = state.board,
      rows = [], i, row, j;

  for (i = 0; i < state.board.height; i++) {
    row = [];
    for (j = 0; j < state.board.width; j++) {
      row.push('•');
    }
    rows.push(row);
  }

  var ships = [state.position];
  ships.forEach(function(position) {
    console.log(position);
    board.reverseWalk(position, position.dir, position.size, function(x, y, axis) {
      rows[y][x] = (x === position.x && y === position.y) ? 'X' : (axis === 'x' ? '-' : '|');
    });
  });
  
  if (state.shooting.aiming) {
    rows[state.shooting.y][state.shooting.x] = '⁘'
  }

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
  stdio.setRawMode(false);
  process.stdout.flush();
  process.stdout.write('\x1b[0G');
  console.log.apply(console, arguments)
  process.stdout.flush();
  stdio.setRawMode(true);
}

// Create server
var server = new ServerProxySocket('localhost', 3001);

// Create and register client
var client = new Client(server);
server.register(client);

server.on('error', function(err) {
  output('connection error: ' + err)
});
server.on('closed', function() {
  output('client terminated');
  stdio.setRawMode(false)
  stdin.destroy();
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
      output('Failed to login: %s', err)
      return terminate();
    }

    output('successfull login for %s: %j', clientName, clientState);

    clientState.shooting = {
      aiming: false,
      x: Math.floor(clientState.board.width / 2),
      y: Math.floor(clientState.board.height / 2),
    }

    // Setup stdin
    stdin.setEncoding('utf8');
    stdio.setRawMode(true);

    stdin.on('data', function (chunk) {
      // Terminate (Ctrl-C)
      if (chunk === '\u0003') {
        return terminate();
      }

      // Move (arrow keys)
      [ ['\u001b[A', 'north'],
        ['\u001b[B', 'south'],
        ['\u001b[C', 'east'],
        ['\u001b[D', 'west']
      ].forEach(function(item) {
        if (chunk === item[0]) {
          if (clientState.shooting.aiming) {
            var next = clientState.board.step(clientState.shooting, item[1]);
            clientState.shooting.x = next.x; 
            clientState.shooting.y = next.y;
            drawBoard(clientState)
          } else {
            output('Moving client %s', item[1]);
            client.move(item[1], function(err, result) {
              if (!err) {
                // Update position
                clientState.position = result.position;
                clientState.position.dir = result.direction;
                drawBoard(clientState)
              }
            })
          }
        }
      });

      // Shoot-mode (Space)
      if (chunk === ' ') {
        if (clientState.shooting.aiming) {
          client.shoot(clientState.shooting, function(err, result) {
            if (err) { return output('Error shooting: %j', err); } 

            output('Shoot result: %j\n', result);
            // Taunt the killed users
            result.forEach(function(user) {
              client.taunt(user, 'Got you!!', function(err, result) {});                
            });
          })
        }
        clientState.shooting.aiming = !clientState.shooting.aiming;
        drawBoard(clientState);
      }

      // output('Received data: ' + sys.inspect(chunk))
    });

    stdin.on('end', function () {
      output('stdio.end')
      terminate();
    });

    // Start by drawing board
    output('Drawing board')
    output(clientState)
    drawBoard(clientState);

  });
});
