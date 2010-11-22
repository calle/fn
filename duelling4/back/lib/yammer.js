/**
 * Yammer Store code
 */
var Store = require('node-yammer-store'),
    EventEmitter = require('events').EventEmitter,
    logger = require('node-logger').logger('yammer'),
    Serial = require('node-wrappers').Serial,
    fs = require('fs'),
    spawn = require('child_process').spawn;

module.exports = function(app) {

  // Setup the store
  var store = new Store('localhost', 5984, 'yammer-store')
  app.set('yammer', new Yammer(store));

}    

var Yammer = function(store) {
  // Invoke EventEmitter constructor
  EventEmitter.call(this);

  this.store = store;
  this.queue = new Serial({ delay: 0 });

  // Caches
  this.messages = {}
  this.users = {}
}

Yammer.prototype.__proto__ = EventEmitter.prototype;

Yammer.prototype.listen = function(from, callback) {
  var self = this;
  
  this.store.setup(function() {
    var stream = self.store.messagesStream(from)

    // Listen for events on stream
    stream.on('message', function(message) {
      logger.info('got message %s', message.id)
      self._convertMessage(message, function(err, message) {
        if (err) return logger.error('error: %o', err);
        self.emit('message', message)
      })
    })

    stream.on('error', function(error) {
      self.emit('error', error);
    })

    stream.on('end', function() {
      self.emit('end', error);
    })

    // Invoke callback
    if (callback) callback()
  });

}

Yammer.prototype._convertMessage = function(message, callback) {
  var self = this;
  
  this._loadUser(message.sender_id, function(err, user) {
    if (err) return callback(err);

    var result = {
      id: message.id,
      from: user,
      content: message.body.plain,
      tags: message.body.plain.match(/#\w+/g) || []
    }
    
    if (message.replied_to_id) {
      self.queue.invoke(function() {

        var next = function(replied_to) {
          self._loadUser(replied_to.sender_id, function(err, replied_to_user) {
            if (err) return callback(err);
            result.inReplyTo = replied_to_user
            callback(null, result);
          })
        }

        if (message.replied_to_id in self.messages) {
          next(self.messages[message.replied_to_id]);
        } else {
          self.store.message(message.replied_to_id, function(err, replied_to) {
            if (err) return callback(err);
            next(replied_to)
          })
        }
      })
    } else {
      callback(null, result);
    }
  })
}

Yammer.prototype._loadUser = function(user_id, callback) {
  var self = this,
      users = this.users;

  this.queue.invoke(function() {
    if (user_id in users) return callback(null, users[user_id]);

    self.store.user(user_id, function(err, user) {
      if (err) return callback(err);

      var suffix = user.mugshot_url.replace(/.*\.([^.]+)$/, '$1').toLowerCase();
      var filename = 'public/images/users/' + user.id + '.'  + suffix;
      var avatar_url = '/images/users/' + user.id + '.'  + suffix;

      var result = {
        id: user.id,
        username: user.name,
        name: user.full_name,
        avatar: avatar_url
      };

      var next = function(err) {
        if (err) return callback(err);

        // Cache it
        users[user_id] = result;
        callback(null, result);
      }

      // Check for image
      fs.stat(filename, function(err, stat) {
        if (err || !stat || !stat.isFile()) {
          logger.info('User %s has no image, will download', user.full_name)
          self._downloadImage(filename, user.mugshot_url, next);
        } else {
          logger.debug('User %s already has image', user.full_name)
          next();
        }
      })
    })
  })
}

Yammer.prototype._downloadImage = function(filename, url, callback) {
  var wget = spawn('wget', ['-O', filename, url]);
  wget.on('exit', function(code) {
    if (code === 0) {
      logger.info('User image saved at %s', filename)
      callback(null, filename)
    } else {
      logger.info('Wget failed: %s', code)
      callback('wget failed: ' + code);
    }
  })
}