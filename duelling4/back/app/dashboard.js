var logger = require('node-logger').logger('dashboard'),
    async  = require('async');

module.exports = function(app) {

  // Holder for messages
  var messages = [];

  // Epoc when we start counting messages
  var fromId = 0;

  var db = app.set('db'),
      socket = app.set('socket'),
      yammer = app.set('yammer'),
      processor = app.set('processor');

  // Setup events

  socket.on('connection', function(client) {
    logger.info('client %s connected', client.sessionId);

    // Send latest 7 messages
    messages.slice(-7).forEach(function(message) {
      client.send({ message:message })
    })

    client.on('message', function(message) {
      logger.debug('client %s message: %j', client.sessionId, message)
      // Ignore all incoming messages
    })

    client.on('disconnect', function() {
      logger.info('client %s disconnected', client.sessionId)
    })
  })

  yammer.on('message', function(message) {
    logger.info('got message %s', message.id)
    db.save(message, function() {
      messages.push(message);
      socket.broadcast({ message:message }); 
    })
  })

  yammer.on('error', function(error) {
    logger.error('error: %o', error);
  })

  yammer.on('end', function() {
    logger.info('yammer stream is closed')
  })  

  // Setup processor

  
  setInterval(function() {
    logger.info('processing %d messages', messages.length)
    processor.processMessages(messages, function(err, score) {
      if (err) return logger.error('error: %o', error);
      logger.info('broadcasting score')
      socket.broadcast({ score:score });
    })
  }, 10000)

  // Setup database and load initial data

  db.setup(function() {
    db.messages(function(err, result) {
      if (err) return logger.err('error: %o', err);

      logger.info('got %d messages from database', result.total)
      if (result.messages.length > 0) {
        messages.unshift.apply(messages, result.messages);
        logger.info('max id is: ' + result.messages[result.messages.length - 1].id);
        yammer.listen(result.messages[result.messages.length - 1].id)
      } else {
        yammer.listen(0)
      }
    })
  });

};
