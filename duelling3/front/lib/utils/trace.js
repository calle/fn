/**
 * Trace method for logging
 */

/*
 * External interface 
 */

var trace = module.exports = function() {
  if (enabled) {
    var args = Array.prototype.slice.apply(arguments);
    output(format(args.shift(), args));
  }
}

trace.prefix = function() {
  var prefixArgs = Array.prototype.slice.apply(arguments),
      prefix = prefixArgs.shift();

  return function() {
    var args = Array.prototype.slice.apply(arguments);
    
    if (typeof(prefix) === 'function') {
      // Invoke prefixFormat if it's a function
      var current = prefix.apply(this);
      // Invoke trace with current prefix
      trace.apply(this, [current.shift() + args.shift()].concat(current).concat(args));
    } else {
      // Invoke trace
      trace.apply(this, [prefix + args.shift()].concat(prefixArgs).concat(args));
    }
  };
}

trace.disable = function() {
  enabled = false;
}

trace.setOutput = function(_output) {
  output = _output;
}

/*
 * Config
 */

var enabled = true;

var output = function(string) {
  console.log(string);
}

/*
 * Util methods
 */

var format = function(format, data) {
  return format.replace(/%([sdj])/, function(_, type) {
    var value = data.shift();
    switch (type) {
      case 's':
        return value === undefined ? '' : value.toString();
      case 'd':
        if (!(typeof(value) === 'number')) throw new Error('%d argument must be number in: ' + format);
      case 'j':
        return JSON.stringify(value);
    }
    throw new Error('Invalid format in: ' + format);
  });
}
