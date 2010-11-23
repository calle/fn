var logger = require('node-logger').logger('dashboard'),
    async  = require('async');

module.exports = function(app) {

  // Cache messages and score
  var messages = [],
      lastScore;

  var db = app.set('db'),
      socket = app.set('socket.server'),
      yammer = app.set('yammer'),
      processor = app.set('processor');

  // Setup events

  socket.on('connection', function(clientId, client) {
    client.on('message', function(message) {
      if (message.login) {
        if (message.login !== 'blodpudding') {
          client.send({ login:'fail' })
        } else {
          // Send login response
          client.send({ login:'ok' })

          // Add to server socket to receive broadcasts
          socket.add(clientId, client)

          // Send latest 7 messages
          messages.slice(-7).forEach(function(message) {
            client.send({ message:message })
          })

          // Send latest score
          if (lastScore) client.send({ score:lastScore });
        }
      }
      // Ignore other incoming messages
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

  var processMessages = function() {
    logger.debug('processing %d messages', messages.length)
    processor.processMessages(messages, function(err, score) {
      if (err) return logger.error('processing error: %o', err);

      lastScore = score;

      logger.debug('broadcasting score')
      socket.broadcast({ score:score });
    })
  };
  setInterval(processMessages, 10000);

  // Setup database and load initial data

  db.setup(function() {
    db.messages(67753442, function(err, result) {
      if (err) return logger.err('error: %o', err);

      logger.info('got %d (of %d) messages from database', result.messages.length, result.total)
      if (result.messages.length > 0) {
        messages.unshift.apply(messages, result.messages);
        logger.info('max id is: ' + result.messages[result.messages.length - 1].id);
        yammer.listen(result.messages[result.messages.length - 1].id)
        processMessages();
      } else {
        yammer.listen(0)
      }
    })
  });

};

