/**
 * Verifies yubikey input
 */

var sys = require('sys'),
    http = require('http'),
    url = require('url'),
    sprintf = require('./sprintf'),
    crypto = require('crypto')

var config = {
  server: 'https://api2.yubico.com/wsapi/2.0/verify'
}

module.exports = function(app) {

  // Setup yubikey
  app.set('yubikey', new Yubikey({ mock:app.set('env') === 'development' }));

}    

var Yubikey = function(options) {
  this.mock = options.mock
  if (this.mock) { return }
 
  var remote = url.parse(options.server || config.server)
    , proxyConfig = options.proxy || process.env['http_proxy']
    , proxy = (proxyConfig) ? url.parse(proxyConfig) : null
    , https = (proxy || remote).protocol === "https:"
    , port = (proxy || remote).port || (https ? 443 : 80)
    , hostname = (proxy || remote).hostname

  this.path = proxy ? remote.href : remote.pathname
  this.headers = {
    'Host': remote.hostname
  };
  this.client = http.createClient(port, hostname, https)
}

Yubikey.prototype.verify = function(otp, callback) {

  if (this.mock) {
    setTimeout(function() {
      callback(null, otp == "asdf")
      callback = null;
    }, 400)
    return
  }

  var nonce = crypto.createHash('md5').update(new Date().getTime().toString()).digest('hex')
  var path = sprintf('%s?id=1&nonce=%s&otp=%s', this.path, nonce, otp)
  var req = this.client.request('GET', path, this.headers)

  req.on('response', function (res) {
    var content = ''
    res.setEncoding('ascii');
    res.on('data', function (chunk) { content += chunk })
    res.on('end', function () {
      // "Parse" response
      var response = {}
      content.split(/\r?\n/).forEach(function(line) {
        if (!line) return
        var parts = line.split(/=/)
        response[parts[0]] = parts[1]
      })
      // Validate response
      var valid = 
        response.otp == otp &&
        response.nonce == nonce &&
        response.status == "OK"
      // Invoke callback
      if (callback) {
        callback(null, valid, response.status, response)
        callback = null
      }
    })
  })
  req.on("error", function (err) {
    if (callback) {
      callback(err)
      callback = null
    }
  })
  req.on("close", function () {
    if (callback) {
      callback(new Error('Connection closed unexpectedly'))
      callback = null
    }
  })

  // Send request
  req.end()

}