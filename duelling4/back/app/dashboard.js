var Wrappers = require('node-wrappers'),
    logger = require('node-logger').logger('dashboard');

module.exports = function(app) {

  // Cache messages and score
  var messages = [],
      score = { good:[], bad:[] };

  var db = app.set('db'),
      socketIO = app.set('socket.server'),
      yammer = app.set('yammer'),
      processor = app.set('processor'),
      yubikey = app.set('yubikey');

  // Setup socketIO

  var login = {
      success: function(clientId, client) {
        logger.info('client %s login success')

        // Send login response
        client.send({ login:'ok' })

        // Add to socketIO so clients receives broadcasts
        socketIO.add(clientId, client)

        // Send latest 7 messages
        messages.slice(-7).forEach(function(message) {
          client.send({ message:message })
        })

        // Send latest score
        client.send({ score:score });
      },
      fail: function(clientId, client) {
        logger.info('client %s login fail')
        client.send({ login:'fail' })
      }
  }

  socketIO.on('connection', function(clientId, client) {
    client.on('message', function(message) {
      if (message.login) {
        var password = message.login;
        logger.info('login attempt by %s, password: %s', clientId, password)

        if (password === 'blodpudding') {
          login.success(clientId, client)
        } else if (password.length >= 32){
          // Try yubikey verification
          logger.debug('yubikey attempt by %s', clientId, password)
          yubikey.verify(password, function(err, valid, status, response) {
            logger.debug('yubikey result: err=%j, valid=%j, status=%j, response=%s', err, valid, status, response)
            if (valid) {
              var id = password.length > 32 ? password.substring(0, password.length - 32) : password;
              logger.info('yubikey login: %s', id)
              login.success(clientId, client)
            } else {
              login.fail(clientId, client);
            }
          })
        } else {
          login.fail(clientId, client);
        }
      }
    })
  })

  // Setup yammer

  yammer.on('message', function(message) {
    logger.info('got message %s', message.id)

    db.save(message, function() {
      messages.push(message);
      socketIO.broadcast({ message:message });

      // Invoke processor to update score
      processMessages();
    })
  })

  yammer.on('error', function(error) {
    logger.error('error: %o', error);
  })

  yammer.on('end', function() {
    logger.info('yammer stream is closed')
  })  

  // Setup processor

  // Make processing serial
  var serialProcessor = Wrappers.Serial(processor)

  var processorResult = null,
      filteredUsers = {},
      userPointsAdjustments = {};

  var processMessages = function() {
    logger.debug('processing %d messages', messages.length)
    serialProcessor.processMessages(messages, function(err, result) {
      if (err)
        return logger.error('processing error: %o', err)
      processorResult = result;
      recalculateScore();
    })
  }

  // Invoke processing messages at least every 10 s
  setInterval(processMessages, 10000)

  var recalculateScore = function() {
    if (!processorResult) return;

    logger.debug('recalculate score, filteredUsers:%j, userPointsAdjustments:%j', filteredUsers, userPointsAdjustments)

    // Adjust result according to admin filters and adjustments
    var result = processorResult.filter(function(item) {
      return !(item.user.id in filteredUsers);
    }).map(function(item) {
      var points = item.points;
      if (item.user.id in userPointsAdjustments) {
        points += userPointsAdjustments[item.user.id];
      }
      return {
        user:   item.user,
        points: points
      }
    })

    // Sort by points
    result.sort(function(a, b) { return a.points - b.points; })

    // Divide in good / bad lists
    var good = result.slice(-10).reverse()
    var bad  = result.slice(0, 10)

    // Update score
    score = { good:good, bad:bad };

    // Finally broadcast new score
    logger.debug('broadcasting score')
    socketIO.broadcast({ score:score });
  }

  // Setup database and load initial data

  db.setup(function() {
    var firstMessage = 67753442;
    db.messages(firstMessage, function(err, result) {
      if (err) return logger.err('error: %o', err);

      logger.info('got %d (of %d) messages from database', result.messages.length, result.total)
      if (result.messages.length > 0) {
        // Append messages
        messages.unshift.apply(messages, result.messages);
        // Start initial procsesing
        processMessages();
        // Listen for more incoming messages
        yammer.listen(result.messages[result.messages.length - 1].id)
      } else {
        // Just listen for new incoming messages
        yammer.listen(firstMessage)
      }
    })
  });

  //
  // Admin interface
  //

  var loggedIn = function(req, res, next) {
    if (req.session && req.session.loggedId) {
      next();
    } else {
      res.render('login.jade', { locals:{ error: ''} });
    }
  }

  app.post('/admin/login', function(req, res) {
    var username = req.param('username'),
        password = req.param('password');

    if (username !== 'fnnl' || password !== 'yrruc')
      return res.render('login.jade', { locals:{ error: 'Felaktig inloggning'} });

    logger.info('login user "%s"', username)
    req.session.loggedId = true;
    res.redirect('/admin')
  });

  app.post('/admin/logout', function(req, res) {
    if (req.session) delete req.session.loggedId;
    res.redirect('/');
  });

  app.get('/admin', loggedIn, function(req, res, next) {
    db.users(function(err, users) {
      if (err) logger.error('db.users() error: %o', err);

      users.forEach(function(user) {
        user.blocked = (user.id in filteredUsers);
        user.adjustment = userPointsAdjustments[user.id]
      })

      // Sort by name
      users.sort(function(a, b) { return a.name < b.name ? -1 : 1; })

      res.render('admin.jade', { locals:{
        users: users || []
      }});
    })
  });

  app.post('/admin/user/block', loggedIn, function(req, res) {
    var userId = req.param('user')
    var blocked;

    if (userId in filteredUsers) {
      delete filteredUsers[userId]
      blocked = false
    } else {
      filteredUsers[userId] = userId
      blocked = true
    }

    recalculateScore();

    res.send({ blocked:blocked })
  })

  app.post('/admin/user/adjust', loggedIn, function(req, res) {
    var userId = req.param('user'),
        points = parseFloat(req.param('points'), 10),
        modified = false;

    if (!isNaN(points) && points !== 0) {
      userPointsAdjustments[userId] = points;
      modified = true
    } else {
      points = 0;
      if (userId in userPointsAdjustments) {
        delete userPointsAdjustments[userId]
        modified = true;
      }
    }

    if (modified) {
      recalculateScore();
    }

    res.send({ points:points });
  })

};

