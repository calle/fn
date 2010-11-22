/**
 * Socket IO code
 */
var io = require('socket.io'),
    logger = require('node-logger').logger('socket-io');

module.exports = function(app) {

  // Setup plain socket
  var socket = io.listen(app, { log:logger.debug.bind(logger) }); 
  app.set('socket', socket);

}