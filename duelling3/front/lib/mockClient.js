var Battlefield = require('./battlefield'),
    stdio = process.binding('stdio'), 
    sys = require('sys');



var battlefield = new Battlefield('localhost', 3001, false);

var id1 = '344',
    id2 = "185";

var step = function(direction, position) {
  var x = position.x, y = position.y;
  if (direction === "north") {
    return { x:x, y:y+1 };
  } else if (direction === "south"){
    return { x:x, y:y-1 };
  } else if (direction === "east"){
    return { x:x+1, y:y };
  } else if (direction === "west"){
    return { x:x-1, y:y };
  }
}

var walk = function(position, callback) {
  var current = position;
  for (var i=0; i < position.size; i++) {
    callback(current.x, current.y);
    current = step(position.direction, current);
  }
};

var drawBoard = function(board, ships) {
  var rows = [], i, row, j;
  for (i = 0; i < board.height; i++) {
    row = [];
    for (j = 0; j < board.width; i++) { 
      row.push('¤');
    }
    rows.push(row);
  }
  
  shift.forEach(function(position) {
    walk(position, function(x, y) {
      rows[y][x] = 'X';
    });
  });
  
  stdio.setRawMode(false);
  console.log(rows.map(function(row) { return row.join(''); }).join('\n'));
  stdio.setRawMode(true);
};

var callbacks = function(login) {
  return {
    login: login,
    update: function(message) {
      console.log('got update: ' + message);
    },
    end: function() {
      console.log('client terminated');
    }
  }
};

battlefield.login(id1, 'calle', callbacks(function(err, state1) {
  console.log('successfull login calle: %j', state1);
  
  var stdin = process.openStdin();

  stdio.setRawMode(true);
  
  stdin.setEncoding('utf8');

  var terminate = function() {
    battlefield.logout(id1, function() {
      stdio.setRawMode(false);
      console.log('\nterminate: logged out')
      stdin.destroy();
    });
  }
  
  stdin.on('data', function (chunk) {
    if (chunk === '\u0003') { // Ctrl-C
      console.log('Received Ctrl-C');
      return terminate();
    }
    console.log('Received data: ' + sys.inspect(chunk))
  });

  stdin.on('end', function () {
    console.log('stdio.end')
    terminate();
  });

  
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