var logger = require('node-logger').logger('snowglobe');

module.exports = function(app) {

  var messages = app.set('messages');

  var loggedIn = function(req, res, next) {
    if (req.session && req.session.loggedId) {
			logger.info("User logged in")
      next();
    } else {
			logger.info("User not logged in")
      res.render('login.jade', { locals:{ error: ''} });
    }
  }

  app.post('/control/login', function(req, res) {
    var username = req.param('username'),
        password = req.param('password');

    if (username !== 'fnnl' || password !== 'yrruc')
      return res.render('login.jade', { locals:{ error: 'Felaktig inloggning'} });

    logger.info('login user "%s"', username)
    req.session.loggedId = true;
    res.redirect('/control')
  });

  app.post('/control/logout', function(req, res) {
    if (req.session) delete req.session.loggedId;
    res.redirect('/');
  });

  app.get('/control', loggedIn, function(req, res, next) {
		logger.debug("Rendering controller")
    return res.render('controller.jade', { locals:{
    }});
  });

  app.post('/control/message', loggedIn, function(req, res, next) {
    messages.message(req.param('message'));
    res.redirect('/control');
  });

  app.post('/control/action', loggedIn, function(req, res, next) {
    try {
      var action = JSON.parse(req.param('action'));
      messages.action(action);
    } catch (e) {
      logger.info('failed to parse action (%s): %s', req.param('action'), e);
    }
    res.redirect('/control');
  });

};
