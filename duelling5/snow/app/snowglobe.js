var logger = require('node-logger').logger('snowglobe');

module.exports = function(app) {

  var socketIO = app.set('socket.server'),
      messages = app.set('messages'),
      yubikey  = app.set('yubikey');

  // Setup socketIO

  socketIO.on('connection', function(clientId, client) {
    logger.info('Client %s connected', clientId)

    // Add to socketIO so clients receives broadcasts
    socketIO.add(clientId, client)

  })

  // Setup messages

  messages.on('message', function(content) {
    logger.info('got message %s', content)

    // Check for #fnnl in message
    // if (!/#fnnl/.test(content)) {
    //   logger.info('Ignore message wihtout #fnnl')
    //   return;
    // }

    // Determine action from content
    var action = actionFromMessage(content);

    if (action) {
      logger.info('broadcast action %j', action)
      socketIO.broadcast( { action:action } )
    } else {
      logger.info('no action found for message')
    }
  })

  messages.on('action', function(action) {
    logger.info('got action %j', action)
    if (action) {
      logger.info('broadcast action')
      socketIO.broadcast( { action:action } )
    } else {
      logger.info('no action')
    }
  })

  var actionFromMessage = function(message) {

    if (/fisk/i.test(message) || /brainfuck/i.test(message))
      return { type:'fish' }

    if (/snögubbe/i.test(message))
      return { type:'image', url:'/images/snowman.png', width:294, height:339 };

    if (/netlight/i.test(message))
      return { type:'image', url:'/images/netlight.gif', width:266, height:65 };

    if (/larsa/i.test(message))
      return { type:'slow' }

    if (/sommar/i.test(message))
      return { type:'snow', value:'off' };

    if (/vinter/i.test(message))
      return { type:'snow', value:'on' };

    if (/functional/i.test(message))
      return { type:'primes', value:'on' };

    if (/java/i.test(message))
      return { type:'primes', value:'off' };

    // Default to search for the message
    var query = message.replace(/\s*#?fnnl\s*/g, '')
    if (/^\s*$/.test(query)) {
      return null
    } else {
      return { type:'search', query:query }
    }
  }

};
