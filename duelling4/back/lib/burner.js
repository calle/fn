var net = require('net')

module.exports = function(app) {

  app.set('Burner', Burner);

}

var Burner = {
  alert: function(callback) {
    var stream = net.createConnection(4444, 'burner.servebeer.com');

    stream.on('connect', function() {
      stream.write('alert\n', 'ascii')
      setTimeout(function() {
        stream.end();
        stream.destroy();
      }, 500)
      if (callback) {
        callback(null, 'done')
        callback = null
      }
    })

    stream.on('error', function(err) {
      if (callback) {
        callback(err)
        callback = null
      }
    })

    stream.on('close', function() {
      if (callback) {
        callback('closed')
        callback = null
      }
    })    
  }
}


Burner.alert(function(err, done) {
  if (err) return console.error(err)
  console.log(done)
})