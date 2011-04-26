/**
 * Configuration
 */
var express = require('express');

module.exports = function(app) {

  var root = __dirname + '/..'

  app.configure(function(){
    app.set('views', root + '/views');
    app.use(express.cookieParser());
    app.use(express.session({secret: "fnnl"}));
    app.use(express.bodyParser());
    app.use(express.methodOverride());
    app.use(express.compiler({ src: root + '/public', enable: ['less'] }));
    app.use(app.router);
		app.use(express.logger({ format: '\x1b[1m:method\x1b[0m \x1b[33m:url\x1b[0m :response-time' }));
    app.use(express.static(root + '/public'));
  });

  app.configure('development', function(){
    app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
  });

  app.configure('production', function(){
    app.use(express.errorHandler());
  });

}