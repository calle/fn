var Battlefield = require('./battlefield'),
    stdio = process.binding('stdio'),
    sys = require('sys');

var battlefield = new Battlefield('localhost', 3001, true);
var clientName = process.argv[2] || 'Calle';
var clientId = Math.floor(Math.random() * 1000);

var stdin = process.openStdin();

var coords = {
  north: { axis: 'y', value:  1 },
  south: { axis: 'y', value: -1 },
  east:  { axis: 'x', value:  1 },
  west:  { axis: 'x', value: -1 },
}

var walk = function(board, position, callback) {
  var coord = coords[position.direction],
      current = { x: position.x, y: position.y },
      board_size = board[coord.axis === 'x' ? 'width' : 'height']

  // Start from tail of ship and to in direction to front
  current[coord.axis] -= coord.value * position.size

  for (var i=0; i < position.size; i++) {
    current[coord.axis] = (current[coord.axis] + coord.value + board_size) % board_size;
    callback(current.x, current.y);
  }
};

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
    var coord = coords[position.direction];
    walk(board, position, function(x, y) {
      rows[y][x] = (x === position.x && y === position.y) ? 'X' : (coord.axis === 'x' ? '-' : '|');
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
  console.log.apply(console, arguments)
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
          var coord = coords[item[1]],
              board_size = clientState.board[coord.axis === 'x' ? 'width' : 'height']
          clientState.shooting[coord.axis] = 
            (clientState.shooting[coord.axis] + coord.value + board_size) % board_size;
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

    // Shoot (Space)
    if (chunk === ' ') {
      battlefield.shoot(clientId, clientState.position, function(err, result) {
        if (!err) output('Shoot result: %j', result);
      })
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
