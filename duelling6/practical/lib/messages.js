/**
 * Messages
 */
var EventEmitter = require('events').EventEmitter,
    logger = require('node-logger').logger('messages');

module.exports = function(app) {

  app.set('messages', new Messages());
}

var Messages = function() {
  // Invoke EventEmitter constructor
  EventEmitter.call(this);
}

Messages.prototype.__proto__ = EventEmitter.prototype;

Messages.prototype.message = function(content) {
  this.emit('message', content);
}

Messages.prototype.action = function(action) {
  this.emit('action', action);
}
