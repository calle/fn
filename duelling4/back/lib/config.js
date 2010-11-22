/**
 * Configuration
 */
var express = require('express');

module.exports = function(app) {

  var root = __dirname + '/..'
  
  app.configure(function(){
    app.set('views', root + '/views');
    app.use(express.cookieDecoder());
    app.use(express.session());
    app.use(express.bodyDecoder());
    app.use(express.methodOverride());
    app.use(express.compiler({ src: root + '/public', enable: ['less'] }));
    app.use(app.router);
    app.use(express.staticProvider(root + '/public'));
  });

  app.configure('development', function(){
    app.use(express.errorHandler({ dumpExceptions: true, showStack: true })); 
  });

  app.configure('production', function(){
    app.use(express.errorHandler()); 
  });

}