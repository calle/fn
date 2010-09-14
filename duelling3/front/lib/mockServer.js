var net = require('net');
var clients = {};

var server = net.createServer(function (stream) {
  stream.setEncoding('ascii');
  stream.on('connect', function () {
    console.log("Client connected");
  });
  stream.on('data', function (data) {
    var parts = data.split(/:/, 2);
    var id = parts[0];
    var command = parts[1];
    console.log("Received data: " + data);
    
    if (command.match(/^login/)) {
      stream.write("response:" + id + ":0,0,east,3,;");
    } else if (command.match(/^move/)) {
      stream.write("response:" + id + ":2,3,north,3;");
    } else if (command.match(/^shoot/)) {
      stream.write("response:" + id + ":miss;")
    } else if (command.match(/^taunt/)) {
      stream.write("response:" + id + ":ok;");
    } else if (command.match(/^logout/)) {
      stream.write("response:" + id + ":ok;");
    }
    stream.flush();
  });
  stream.on('end', function () {
    stream.end();
    console.log("Client died (logged out)");
  });
});
server.listen(3001, 'localhost');