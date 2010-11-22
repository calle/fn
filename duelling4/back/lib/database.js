/**
 * Database
 */
var couchdb = require('couchdb'),
    async   = require('async'),
    setup   = require('./database-setup')
    logger  = require('node-logger').logger('Database');

//
// External interface
// 

module.exports = function(app) {
  app.set('db', new Database(app.set('env')));
}

var Database = module.exports.Database = function(env) {

  // Connect to database
  this.db = couchdb.createClient(5984, 'localhost').db('fn-duelling4');
  this.env = env;

}

Database.prototype = {

  setup: function(callback) {
    setup.setupDatabase(this.db, this.env, callback || function() {})
  },

  messages: function(callback) {
    logger.trace('messages(...)');
    this.db.view('data', 'messages-by-id', {}, function(err, data) {
      if (err) return callback(err);
      callback(null, {
        start: data.offset || 0,
        total: data.total_rows || 0,
        messages: (data.rows || []).map(function(row) { return row.value })
      });
    });
  },

  save: function(message, callback) {
    logger.trace('save(%j)', message);

    if (!message._id)  message._id  = 'message-' + message.id
    if (!message.type) message.type = 'message'

    this.db.saveDoc(message, function(err, result) {
      if (err && err.error !== 'conflict') return callback(err);
      callback(null, result && result.ok)
    })
  }

};

// 
// Example code for development
// 

if (!module.parent) {
  console.log("Execute tests on db")

  var sys = require('sys');
  var Logger = require('node-logger')
  Logger.setLevel(Logger.TRACE)

  var db = new Database('development', function() {
    console.log('run db tests')

    db.messages(function(err, result) {
      console.log(result.messages.map(function(m) { return m.id }).join(','));
    })
  });

}