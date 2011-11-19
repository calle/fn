var EventEmitter = require('events').EventEmitter
  , Player       = require('./player')
  , Ball         = require('./ball');

var Game = module.exports = function(options) {
  EventEmitter.call(this);

  this.objects      = {};
  this.nextObjectId = 0;

  this.board   = {
    left   : 0
  , top    : 0
  , width  : options.width  || 100
  , height : options.height || 100
  };
  this.board.right  = this.board.left + this.board.width;
  this.board.bottom = this.board.top  + this.board.height;

  this.score = {
    left  : 0
  , right : 0
  };
};

Game.prototype = new EventEmitter();

Game.prototype.tick = function() {
  var time = Date.now();
  _.each(this.objects, function(object) {
    object.update(time, this);
  }, this);
};

Game.prototype.getState = function() {
  return {
    objects      : _.map(this.objects, function(object) { return object.getState(); })
  , nextObjectId : this.nextObjectId
  , board        : this.board
  , score        : this.score
  };
};

Game.prototype.updateState = function(state) {
  var toRemove = _.clone(this.objects)
    , score    = this.score;

  _.each(state.objects, function(state) {
    var existing = this.objects[state.id];
    if (existing) {
      delete toRemove[state.id];
      existing.updateState(state);
    } else {
      this.addObjectByState(state);
    }
  }, this);

  _.each(toRemove, function(object, id) {
    if (object.type === Ball.TYPE) {
      this.removeBall(id);
    } else if (object.type === Player.TYPE) {
      this.removePlayer(id);
    }
  }, this);

  this.nextObjectId = state.nextObjectId;

  this.board = state.board;
  this.score = state.score;

  if (score.left !== this.score.left || score.right !== this.score.right) {
    this.emit('score updated', this.score);
  }
};

Game.prototype.getBoard = function() {
  return this.board;
};

Game.prototype.getObjects = function() {
  return this._getObjectsOfType();
};

Game.prototype.getObject = function(id) {
  return this._getObjectOfType(id);
};

Game.prototype.addObjectByState = function(state) {
  var constructor;
  if (state.type === Player.TYPE) {
    constructor = Player;
  } else if (state.type === Ball.TYPE) {
    constructor = Ball;
  }
  if (constructor) {
    var object = this._addObject(constructor, state.id, {});
    if (object) {
      object.updateState(state);
      if (state.type === Player.TYPE) {
        this.emit('add player', object);
      } else if (state.type === Ball.TYPE) {
        this.emit('add ball', object);
      }
    }
  }
};

Game.prototype.removeObject = function(id) {
  console.log('Game.removeObject(%s)', id);
  var object = this._removeObjectOfType(id);
  if (object) {
    if (object.type === Player.TYPE) {
      this.emit('remove player', object);
    } else if (object.type === Ball.TYPE) {
      this.emit('remove ball', object);
    }
  }
  return object;
};

Game.prototype.getObjectStates = function(type) {
  return _.map(this._getObjectsOfType(type), function(object) { return object.getState(); });
};

Game.prototype.updateObjectStates = function(states) {
  // Only update existing objects, ignore extra/removed
  _.each(states, function(state) {
    var existing = this.objects[state.id];
    if (existing) {
      existing.updateState(state);
    }
  }, this);  
};

Game.prototype.getPlayers = function() {
  return this._getObjectsOfType(Player.TYPE);
};

Game.prototype.getPlayer = function(id) {
  return this._getObjectOfType(id, Player.TYPE);
};

Game.prototype.addPlayer = function(name, attributes) {
  // Normalize attributes
  if (!attributes) attributes = {};
  attributes.name = name;

  // Add player to list of objects and emit
  var player = this._addObject(Player, null, attributes);
  if (player) {
    this.emit('add player', player);
    console.log('Game.addPlayer(%s, "%s")', player.id, player.name);
  }
  return player;
};

Game.prototype.removePlayer = function(id) {
  console.log('Game.removePlayer(%s)', id);
  var player = this._removeObjectOfType(id, Player.TYPE);
  if (player) {
    this.emit('remove player', player);
  }
  return player;
};

Game.prototype.getBalls = function() {
  return this._getObjectsOfType(Ball.TYPE);
};

Game.prototype.getBall = function(id) {
  return this._getObjectOfType(id, Ball.TYPE);
};

Game.prototype.addBall = function(attributes) {
  // Normalize attributes
  if (!attributes) attributes = {};

  // Add player to list of objects and emit
  var ball = this._addObject(Ball, null, attributes);
  if (ball) {
    this.emit('add ball', ball);
    console.log('Game.addBall(%s)', ball.id);

    // Listen for dead events and emit on game as well
    var self = this;
    ball.on('dead', function(edge) {
      self.score[edge] += 1;
      self.emit('ball dead', ball, edge);
      self.emit('score updated', self.score);
    });
  }
  return ball;
};

Game.prototype.removeBall = function(id) {
  console.log('Game.removeBall(%s)', id);
  var ball = this._removeObjectOfType(id, Ball.TYPE);
  if (ball) {
    this.emit('remove ball', ball);
    // TODO: Remove listener for dead events
  }
  return ball;
};

Game.prototype._addObject = function(constructor, id, attributes) {
  // Validate id argument
  if (!id) {
    id = this.nextObjectId;
    this.nextObjectId += 1;
  } else if (id >= this.nextObjectId) {
    this.nextObjectId = id + 1;
  }

  // Create object
  var object = new constructor(id, attributes);
  if (object) {
    this.objects[id] = object;
    this.emit('add object', object);
  }
  return object;
};

Game.prototype._getObjectsOfType = function(type) {
  if (!type) return _.values(this.objects);
  return _.filter(this.objects, function(object) { return object.type === type; });
};

Game.prototype._getObjectOfType = function(id, type) {
  var object = this.objects[id];
  if (object && object.id === id && (!type || object.type === type)) {
    return object;
  } else {
    return null;
  }
};

Game.prototype._removeObjectOfType = function(id, type) {
  var object = this.objects[id];
  if (object && object.id === id && (!type || object.type === type)) {
    delete this.objects[id];
    this.emit('remove object', object);
    return object;
  } else {
    return false;
  }
};