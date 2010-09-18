var Battlefield = require('./battlefield'),
    stdio = process.binding('stdio'), 
    sys = require('sys');



var battlefield = new Battlefield('localhost', 3001, false);

var id1 = '344',
    id2 = "185";

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
  
  stdio.setRawMode(false);
  console.log()
  console.log(rows.reverse().map(function(row) { return row.join(' '); }).join('\n'));
  stdio.setRawMode(true);
};

var callbacks = function(login) {
  return {
    login: login,
    update: function(message) {
      console.log('got update: ' + message);
    },
    error: function(err) {
      output('connection error: ' + err)
    },
    end: function() {
      console.log('client terminated');
    }
  }
};

battlefield.login(id1, 'calle', callbacks(function(err, state1) {
  console.log('successfull login calle: %j', state1);

  // Setup stdin
  var stdin = process.openStdin();
  stdio.setRawMode(true);
  stdin.setEncoding('utf8');

  stdin.on('data', function (chunk) {
    if (chunk === '\u0003') { // Ctrl-C
      console.log('Received Ctrl-C');
      return terminate();
    } 
    
    [['\u001b[A', 'north'], ['\u001b[B', 'south'], ['\u001b[C', 'east'], ['\u001b[D', 'west']].forEach(function(item) {
      if (chunk === item[0]) {
        battlefield.move(id1, item[1], function(err, position) {
          if (!err) drawBoard(state1.board, position)
        })
      }
    })
    console.log('Received data: ' + sys.inspect(chunk))
  });
  stdin.on('end', function () {
    console.log('stdio.end')
    terminate();
  });
  var terminate = function() {
    battlefield.logout(id1, function() {
      stdio.setRawMode(false);
      console.log('\nterminate: logged out')
      stdin.destroy();
    });
  }

  // Start by drawing board
  console.log('Drawing board')
  console.log(state1)
  drawBoard(state1.board, [state1.position]);

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