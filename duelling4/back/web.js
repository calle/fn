/**
 * Main entrypoint
 */
var express = require('express'),
    Logger  = require('node-logger');

var app = module.exports = express.createServer();

require('./lib/config')(app);
require('./lib/database')(app);
require('./lib/yammer')(app);
require('./lib/processor')(app);
require('./lib/socket.io')(app);
require('./app/dashboard')(app);

// Only listen on $ node app.js
if (!module.parent) {
  Logger.setLevel(Logger.DEBUG);
  app.listen(process.argv[2] || 3000);
  console.log("Express server listening on port %d", app.address().port)
}
