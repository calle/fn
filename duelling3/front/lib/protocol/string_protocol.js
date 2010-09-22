
/**
 * String protocol, pack and unpack messages to and from strings
 *
 * @param separator -
 *    between two messages
 *
 */
var StringProtocol = module.exports = function() {
  this.messageSepString = '\n';
  this.messageSepRegExp = /\r?\n/;
}

/*
 * External interface
 */

StringProtocol.prototype.packUpdate = function(type, data) {
  return this['pack' + capitalize(type) + 'Update'](data);
}

StringProtocol.prototype.unpackUpdate = function(type, message) {
  return this['unpack' + capitalize(type) + 'Update'](message);
}

StringProtocol.prototype.packRequest = function(type, data) {
  return this['pack' + capitalize(type) + 'Request'](data);
}

StringProtocol.prototype.unpackRequest = function(type, message) {
  return this['unpack' + capitalize(type) + 'Request'](message);
}

StringProtocol.prototype.packResponse = function(type, data) {
  return this['pack' + capitalize(type) + 'Response'](data);
}

StringProtocol.prototype.unpackResponse = function(type, message) {
  return this['unpack' + capitalize(type) + 'Response'](message);
}

/*
 * Updates
 */

StringProtocol.prototype.packUserloginUpdate = function(data) {
  return format('{name}', data);
}

StringProtocol.prototype.unpackUserloginUpdate = function(message) {
  return { name:message };
}

StringProtocol.prototype.packUserlogoutUpdate = function(data) {
  return format('{name}', data);
}

StringProtocol.prototype.unpackUserlogoutUpdate = function(message) {
  return { name:message };
}

StringProtocol.prototype.packUserkilledUpdate = function(data) {
  return format('{name}', data);
}

StringProtocol.prototype.unpackUserkilledUpdate = function(message) {
  return { name:message };
}

StringProtocol.prototype.packTauntedUpdate = function(data) {
  return format('{from}:{message}', data);
}

StringProtocol.prototype.unpackTauntedUpdate = function(message) {
  var parts = message.split(/:/);
  return { 
    from:    parts.shift(), 
    message: parts.join(':') 
  };
}

StringProtocol.prototype.packKilledUpdate = function(data) {
  return format('{by}:{position.x},{position.y}', data);
}

StringProtocol.prototype.unpackKilledUpdate = function(message) {
  var parts = message.split(/:/),
      by = parts.shift(),
      positions = parts.shift().split(/,/);
  return { 
    by: by,
    position: {
      x: parseInt(positions.shift(), 10), 
      y: parseInt(positions.shift(), 10) 
    }
  };
}

/*
 * Requests
 */

StringProtocol.prototype.packLoginRequest = function(data) {
  return data.name;
}

StringProtocol.prototype.unpackLoginRequest = function(message) {
  return { name: message };
}

StringProtocol.prototype.packLogoutRequest = function(data) {
  return '';
}

StringProtocol.prototype.unpackLogoutRequest = function(message) {
  return {};
}

StringProtocol.prototype.packMoveRequest = function(data) {
  return format('{direction}', data);
}

StringProtocol.prototype.unpackMoveRequest = function(message) {
  return { direction: message };
}

StringProtocol.prototype.packShootRequest = function(data) {
  return format('{position.x},{position.y}', data);
}

StringProtocol.prototype.unpackShootRequest = function(message) {
  var parts = message.split(/,/)
  return {
    position: {
      x: parseInt(parts.shift(), 10), 
      y: parseInt(parts.shift(), 10) 
    }
  };
}

StringProtocol.prototype.packTauntRequest = function(data) {
  return format('{name}:{message}');
}

StringProtocol.prototype.unpackTauntRequest = function(message) {
  var parts = message.split(/:/)
  return { 
    name:    parts.shift(), 
    message: parts.join(':') 
  };
}

/*
 * Responses
 */

StringProtocol.prototype.packLoginResponse = function(data) {
  var names = data.clients.join(',')
  return format('{board.width},{board.height},' + 
                '{position.x},{position.y},{position.dir},' +
                '{size},' + 
                names, 
    data);
}

StringProtocol.prototype.unpackLoginResponse = function(message) {
  var parts = message.split(/,/)
  return {
    board: {
      width:  parseInt(parts.shift(), 10),
      height: parseInt(parts.shift(), 10)
    },
    position: {
      x:     parseInt(parts.shift(), 10),
      y:     parseInt(parts.shift(), 10),
    },
    direction: parts.shift(),
    size:      parseInt(parts.shift(), 10),
    clients:   parts.filter(function(part) { return part; })
  };
}

StringProtocol.prototype.packLogoutResponse = function(data) {
  return 'ok';
}

StringProtocol.prototype.unpackLogoutResponse = function(message) {
  return {};
}

StringProtocol.prototype.packMoveResponse = function(data) {
  return format('{position.x},{position.y},{direction}', data);
}

StringProtocol.prototype.unpackMoveResponse = function(message) {
  var parts = message.split(/,/);
  return {
    position: {
      x: parseInt(parts.shift(), 10),
      y: parseInt(parts.shift(), 10),
    },
    direction: parts.shift()
  };
}

StringProtocol.prototype.packShootResponse = function(data) {
  if (data && data.length > 0) {
    return 'kill,' + data.join(',');
  }
  return 'miss'
}

StringProtocol.prototype.unpackShootResponse = function(message) {
  var parts = message.split(/,/),
      status = parts.shift();
  if (status === 'kill') {
    return parts.filter(function(part) { return part; });
  } else {
    return [];
  }
}

StringProtocol.prototype.packTauntResponse = function(data) {
  return 'ok';
}

StringProtocol.prototype.unpackTauntResponse = function(message) {
  return {};
}

StringProtocol.prototype.packErrorResponse = function(data) {
  return format('error:{message}', data);
}

StringProtocol.prototype.unpackErrorResponse = function(message) {
  var parts = message.split(/:/);
  return { message:parts[1] };
}

/*
 * Helper methods
 */

var capitalize = function(string) {
  return string.replace(/(.)(.*)/, function(s,c,cs) { return c.toUpperCase() + cs.toLowerCase(); });
}

var format = function(format, data) {
  return format.replace(/\{([^\}]+)\}/g, function(_, key) {
    var parts = key.split(/\./),
        value = data;

    while (value !== undefined && parts.length > 0) {
      value = value[parts.shift()];
    }

    if (value === undefined) return '';

    return value;
  });
}
