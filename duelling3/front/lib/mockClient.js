var Battlefield = require('./battlefield'),
    stdio = process.binding('stdio'), 
    sys = require('sys');



var battlefield = new Battlefield('localhost', 3001, true);
var clientId = Math.floor(Math.random() * 1000);

var stdin = process.openStdin();

var step = function(board, direction, position) {
  var x = position.x, y = position.y;
  if (direction === "north") {
    y -= 1;
  } else if (direction === "south"){
    y += 1;
  } else if (direction === "east"){
    x -= 1;
  } else if (direction === "west"){
    x += 1;
  }
  return { 
    x: (x + board.width ) % board.width,
    y: (y + board.height) % board.height
  }
}

var walk = function(board, position, callback) {
  var current = position;
  for (var i=0; i < position.size; i++) {
    callback(current.x, current.y);
    current = step(board, position.direction, current);
  }
};

var drawBoard = function(board, ships) {
  var rows = [], i, row, j;
  for (i = 0; i < board.height; i++) {
    row = [];
    for (j = 0; j < board.width; j++) { 
      row.push('-');
    }
    rows.push(row);
  }
  
  if (!Array.isArray(ships)) ships = [ships];
  
  ships.forEach(function(position) {
    walk(board, position, function(x, y) {
      rows[y][x] = (x === position.x && y === position.y) ? '*' : 'X'; 
    });
  });
  
  output('')
  output(rows.reverse().map(function(row) { return row.join(' '); }).join('\n'));
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

battlefield.login(clientId, 'calle', callbacks(function(err, clientState) {
  if (err) {
    output('Failed to login: %s', err)
    return terminate();
  }

  output('successfull login calle: %j', clientState);

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
        battlefield.move(clientId, item[1], function(err, position) {
          if (!err) {
            // Update position
            clientState.position = position;
            // Draw board
            drawBoard(clientState.board, position)
          }
        })
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
  drawBoard(clientState.board, clientState.position);

}));




/*

  battlefield.login(id2, 'olle', callbacks(function(err, state2) {
    console.log('successfull login olle: %j', state2);

    battlefield.shoot(id1, { x:state2.position.x, y:state2.position.y }, function(err, result) {
      console.log('shooing result: %j', result);
      
      battlefield.taunt(id1, 'olle', 'your mama!', function(err, result) {
        console.log('taunt result: %j', result);
        battlefield.logout(id1);
        battlefield.logout(id2);
      });
    });
  }));

*/