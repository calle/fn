/**
 * Messages
 */
var EventEmitter = require('events').EventEmitter,
    logger = require('node-logger').logger('messages');

module.exports = function(app) {

  // Load yammer
  require('./yammer')(app);

  app.set('messages', new Messages(app.set('yammer')));
}

var Messages = function(yammer) {
  // Invoke EventEmitter constructor
  EventEmitter.call(this);

  this.setupYammer(yammer);
}

Messages.prototype.__proto__ = EventEmitter.prototype;

Messages.prototype.setupYammer = function(yammer) {
  var self = this;

  yammer.on('message', function(message) {
    logger.info('got yammer message %j', message);
    self.message(message.content)
  })

  yammer.on('error', function(error) {
    logger.error('yammer error: %o', error);
  })

  yammer.on('end', function() {
    logger.info('yammer stream is closed')
  })

  yammer.listen()
}

Messages.prototype.message = function(content) {
  this.emit('message', content);
}

Messages.prototype.action = function(action) {
  this.emit('action', action);
}
