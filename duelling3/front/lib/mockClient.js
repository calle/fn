var Battlefield = require('./battlefield'),
    stdio = process.binding('stdio'),
    sys = require('sys');

var battlefield = new Battlefield('localhost', 3001, false);
var clientName = process.argv[2] || 'Calle';
var clientId = Math.floor(Math.random() * 1000);

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
    board.reverseWalk(position, position.direction, position.size, function(x, y, axis) {
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
    error: function(err) {
      output('connection error: ' + err)
    },
    end: function() {
      output('client terminated');
      stdio.setRawMode(false)
      stdin.destroy();
    }
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

var terminate = function() {
  battlefield.logout(clientId, function() {
    output('terminate: logged out')
  });
}

battlefield.login(clientId, clientName, callbacks(function(err, clientState) {
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
          battlefield.move(clientId, item[1], function(err, position) {
            if (!err) {
              // Update position
              clientState.position = position;
              drawBoard(clientState)
            }
          })
        }
      }
    });

    // Shoot-mode (Space)
    if (chunk === ' ') {
      if (clientState.shooting.aiming) {
        battlefield.shoot(clientId, clientState.shooting, function(err, result) {
          if (!err) output('Shoot result: %j\n', result);
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

}));
