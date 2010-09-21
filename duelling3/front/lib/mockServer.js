var BattlefieldServer = require('./server/battlefield_server'),
    ServerListener = require('./server/server_listener');

var server = new BattlefieldServer({ boardSize:8 });
var listener = new ServerListener(server);

listener.listen('localhost', 3001);
