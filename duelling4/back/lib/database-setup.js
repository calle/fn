/**
 * Database
 */
var couchdb = require('couchdb'),
    async   = require('async'),
    logger  = require('node-logger').logger('Database-setup');

//
// External interface
// 

exports.setupDatabase = function(db, env, next) {
  logger.trace('setupDatabase')

  var self = this;
  async.waterfall([
     db.exists.bind(db),
     function(exists, next) {
       if (!exists) 
         db.create(next)
       else if (env === 'local')
         removeDesigns(db, next)
       else
         next(null, {})
     },
     function(info, next) {
       logger.info('created or removeDesigns completed: %j', info);
       if (info.ok || env === 'local')
         createDesigns(db, next)
       else
         next(null, {})
     },
     function(info, next) {
       logger.info('createDesigns completed: %j', info);
       next(null, {});
     }
   ], next)
};

var removeDesigns = function(db, next) {
  logger.trace('removeDesigns');

  async.map(['data'], removeDesign.bind(null, db), next)
};
  
var removeDesign = function(db, name, next) {
  db.getDoc('_design/' + name, function(err, doc) {
    console.log('removeDesign: fetched design ' + name + ': ' + doc);
    if (doc && doc._rev)
      db.removeDoc('_design/' + name, doc._rev, next)
    else 
      next(null, { ok:true })
  })
};

var createDesigns = function(db, next) {
  logger.trace('createDesigns')

  async.parallel([
    db.saveDesign.bind(db, 'data', { 
      views: {
        'messages-by-id': {
          map: function(doc) {
            if (doc.type !== 'message') return;
            emit(doc.id, doc)
          }
        }
      }
    })
  ], next)
};

// 
// Example code for development
// 

if (!module.parent) {
  console.log("Execute setup on db")

  var sys = require('sys'),
      Logger = require('node-logger')
  Logger.setLevel(Logger.TRACE)

  var db = couchdb.createClient(5984, 'localhost').db('fn-duelling4');
  exports.setupDatabase(db, 'local', function(err) {
    if (err)
      console.log('Setup failed: ' + err)
    else
      console.log('Setup completed!');
  })
}