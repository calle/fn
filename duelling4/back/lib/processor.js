/**
 * Processor code
 */
var logger = require('node-logger').logger('processor');

module.exports = function(app) {

  // Setup the processor
  var processor = new Processor()
  app.set('processor', processor);

}    

var Processor = function() {
}

Processor.prototype.processMessages = function(messages, callback) {
  var users = {}

  messages.forEach(function(message) {
    if (!(message.from.id in users)) {
      users[message.from.id] = message.from;
      users[message.from.id].seen = 0
    }
    users[message.from.id].seen += 1;
  });

  // points = seen
  var users = Object.keys(users).map(function(user_id) { 
    var user = users[user_id];
    user.points = user.seen
    return user
  });

  // Sort by points
  users.sort(function(a, b) { return a.points - b.points; })

  var good = users.slice(-10)
  var bad  = users.slice(0, 10)

  setTimeout(function() {
    callback(null, { good:good, bad:bad });
  }, 500)
}
