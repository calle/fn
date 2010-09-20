var Server = require('./server');

var server = new Server({ boardSize:8 });

server.listen('localhost', 3001);
