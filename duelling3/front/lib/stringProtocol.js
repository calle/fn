
var StringProtocol = module.exports = function() {
  
  
};

/*
 * Updates
 */

StringProtocol.prototype.packTauntedUpdate = function(data) {
  return format('{from}:{message}', data);
}

StringProtocol.prototype.unpackTauntedUpdate = function(message) {
  var parts = message.split(/:/)
  return { 
    from:    parts.shift(), 
    message: parts.join(':') 
  };
}

StringProtocol.prototype.packKilledUpdate = function(data) {
  return format('{by}:{position.x},{position.y}', data);
}

StringProtocol.prototype.unpackKilledUpdate = function(data) {
  var parts = message.split(/:/),
      by = parts.shift(),
      positions = parts.shift().split(/,/)
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

StringProtocol.prototype.packShootRequest = function(data) {
  return format('{position.x},{position.y}', data)
}

StringProtocol.prototype.unpackShootRequest = function(message) {
  var parts = message.split(/,/)
  return {
    position: {
      x:parseInt(parts.shift(), 10), 
      y:parseInt(parts.shift(), 10) 
    }
  };
}

StringProtocol.prototype.packTauntRequest = function(data) {
  return format('{name}:{message}')
}

StringProtocol.prototype.unpackTauntRequest = function(message) {
  var parts = message.split(/:/)
  return { 
    name:    parts.shift(), 
    message: parts.join(':') 
  };
}

StringProtocol.prototype.packLogoutRequest = function(data) {
  return '';
}

StringProtocol.prototype.unpackLogoutRequest = function(message) {
  return {};
}

/*
 * Replies
 */

StringProtocol.prototype.packErrorReply = function(data) {
  return format('error:{message}', data);
}

StringProtocol.prototype.unpackErrorReply = function(message) {
  return { message: message };
}

StringProtocol.prototype.packLoginReply = function(data) {
  var names = data.clients.join(',')
  return format('{board.width},{board.height},' + 
    '{position.x},{position.y},{position.dir},{size},' + 
    names, data);
}

StringProtocol.prototype.unpackLoginReply = function(message) {
  var parts = message.split(/,/)
  return {
    board: {
      width:  parseInt(parts.shift(), 10),
      height: parseInt(parts.shift(), 10)
    },
    position: {
      x:     parseInt(parts.shift(), 10),
      y:     parseInt(parts.shift(), 10),
      dir:   parts.shift(),
    },
    size:    parseInt(parts.shift(), 10)
    clients: parts.filter(function(part) { return part; })
  };
}

StringProtocol.prototype.packMoveReply = function(data) {
  return format('{position.x},{position.y},{position.dir}', data);
}

StringProtocol.prototype.unpackMoveReply = function(message) {
  var parts = message.split(/,/);
  return {
    position: {
      x:   parseInt(message.shift(), 10),
      y:   parseInt(message.shift(), 10),
      dir: message.shift()
    }
  }
}

StringProtocol.prototype.packShootReply = function(data) {
  if (data.kills.length > 0) {
    return 'kill,' + data.kills.join(',');
  }
  return 'miss'
}

StringProtocol.prototype.unpackShootReply = function(message) {
  var parts = response.split(/,/),
      result = { status: parts.shift() };
  if (result.status === 'kill') {
    result.targets = parts.filter(function(part) { return part; });
  }
  return result;
}

StringProtocol.prototype.packTauntReply = function(data) {
  return 'ok';
}

StringProtocol.prototype.unpackTauntReply = function(message) {
  return {};
}

StringProtocol.prototype.packLogoutReply = function(data) {
  return 'ok';
}

StringProtocol.prototype.unpackLogoutReply = function(message) {
  return {};
}


/*
 * Helper methods
 */

var format = function(format, data) {
  return format.replace(/\{([^\}]+)\}/, function(_, key) {
    var parts = key.split(/\./),
        value = data;

    while (value !== undefined && parts.length > 0) {
      value = value[parts.shift()];
    }

    if (value === undefined) return '';

    return value;
  });
}