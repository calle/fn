/**
 * Main entrypoint
 */
var express = require('express'),
    Logger  = require('node-logger');

var app = module.exports = express.createServer();

require('./lib/config')(app);
require('./lib/socket.io')(app);
require('./lib/messages')(app);

require('./app/controller')(app);
require('./app/snowglobe')(app);

// Only listen on $ node app.js
if (!module.parent) {

  process.on('uncaughtException', function (err) {
    console.log('Caught exception: ' + err);
    if (err) {
      console.log(err);
      console.log(err.message);
      console.log(err.stack);
    }
    process.exit(1);
  });

  Logger.setLevel(Logger.DEBUG);
  app.listen(process.argv[2] || 3000);
  console.log("Express server listening on port %d", app.address().port)
}
