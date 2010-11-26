/**
 * Processor code
 */
var http = require('http'),
    URL  = require('url'),
    logger = require('node-logger').logger('processor');

module.exports = function(app) {

  // Setup the processor
  var processor = new Processor('http://127.0.0.1:8080/kgb/score')
  app.set('processor', processor);

}

var Processor = function(url) {
  this.url = URL.parse(url)
  if (!this.url.port) {
    if (this.url.protocol === "http:")  this.url.port= 80
    if (this.url.protocol === "https:") this.url.port= 443
  }
}

Processor.prototype.processMessages = function(messages, callback) {

  // Extract users by id
  var users = messages.reduce(function(users, message) {
    users[message.from.id] = message.from;
    return users;
  }, {});

  // Convert messages to kgb-server format
  // {"messages":
  //   [
  //    {"from":1, "to":2, "text":"Hej! Hopp! Ange alla!", "time":600, "tags" : []},
  //    {"from":2, "to":1, "text":"Nu får du allt ta och lugna dig...", "time":60, "tags" : []},
  //    {"from":3, "to":1, "text":"Ät kaviar! Fattiglapp", "time":30, "tags" : []},
  //    {"from":1, "to":3, "text":"Kapitalist! Jag äter hellre blodpudding!", "time":5, "tags" : []}
  //   ]
  // }

  var now = Date.now()
  var request = {
    messages: messages.map(function(message) {
      return {
        from: message.from.id,
        // Non-replies get user '0' and destination
        to:   message.inReplyTo ? message.inReplyTo.id : 0,
        text: message.content,
        // Time is number of seconds since now
        time: Math.max(0, Math.round((now - new Date(message.time).getTime()) / 1000)),
        tags: message.tags.map(function(tag) { return tag.replace(/^#/, '') })
      }
    })
  }

  // Invoke external service
  this._invokeProcessor(request, function(err, response) {
    if (err) return callback(err);

    // Remove user '0' from result added above for non-replies
    delete response["0"];

    // Convert response from kgb-server format
    //  {"3":-98.34714538216176,"2":-73.8027249891003,"1":172.14987037126207}
    var result = Object.keys(response).map(function(userId) {
      return {
        user:   users[userId],
        // Normalize points by multiplying by 1000 and rounding
        points: Math.round(response[userId] * 1000)
      }
    })

    // Invoke callback
    callback(null, result);
  })
}

Processor.prototype._invokeProcessor = function(data, callback) {
  // Cleanup data to prevent corner-cases in kgb-server
  //  - remove any messages with from == to
  data.messages = data.messages.filter(function(msg) { return msg.to !== msg.from })
  //  - remove messages if list is empty
  if (!data.messages.length) delete data.messages

  // Util methods
  var done = false,
      result = function(err, value) {
        if (!done) {
          callback(err, value)
          done = true
        }
      }

  // Create client
  var client = http.createClient(this.url.port, this.url.hostname)
  client.on('error', result)

  // Create request
  var req = client.request('POST', this.url.pathname, {
    "Host": this.url.host,
    "Content-Type": "application/json"
  })
  req.end(JSON.stringify(data), 'utf8')

  // Handle response
  req.on('response', function (resp) {
    if (resp.statusCode !== 200)
      return result(new Error('Failed request towards processor, status ' + resp.statusCode))

    var content = ''
    resp.setEncoding('utf8');
    resp.on('data', function (chunk) { content += chunk })
    resp.on('end', function() {
      try {
        result(null, JSON.parse(content))
      } catch (e) {
        result(e)
      }
    })
  });
}