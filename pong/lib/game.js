var EventEmitter = require('events').EventEmitter
  , Player       = require('./player_accelerating')
  , Ball         = require('./ball');

var Game = module.exports = function(options) {
  EventEmitter.call(this);

  _.bindAll(this, '_ballDead');

  this.objects      = {};
  this.nextObjectId = 0;

  this.board   = {
    left   : 0
  , top    : 0
  , width  : (options && options.size && options.size.width) || 100
  , height : (options && options.size && options.size.height) || 100
  };
  this.board.right  = this.board.left + this.board.width;
  this.board.bottom = this.board.top  + this.board.height;

  this.score = 0;

  this.time_step  = (options && options.time_step) || 1/60;
  this.nextUpdate = this.startTime = Date.now();
  this.updates    = 0;
};

Game.prototype = new EventEmitter();

//
// Simple game properties
// 

Game.prototype.getObjects = function() {
  return _.values(this.objects);
};

Game.prototype.getBoard = function() {
  return this.board;
};

Game.prototype.getScore = function() {
  return this.score;
};

//
// State methods
//

Game.prototype.getState = function() {
  return {
    objects      : _.reduce(this.objects, function(result, object) { 
                     result[object.id] = object.getState(); 
                     return result;
                   }, {})
  , nextObjectId : this.nextObjectId
  , board        : this.board
  , score        : this.score
  , time_step    : this.time_step
  };
};

Game.prototype.updateState = function(state) {
  // -- objects
  var currentIds = _.keys(this.objects)
    , newIds     = _.keys(state.objects);

  var add    = _.difference(newIds, currentIds)
    , update = _.intersection(currentIds, newIds)
    , remove = _.difference(currentIds, newIds);

  // console.log('Game.updateState(add=%s, update=%s, remove=%s)', 
  //   JSON.stringify(add), JSON.stringify(update), JSON.stringify(remove));

  // Add new
  _.each(add, function(id) {
    this.addObject(state.objects[id]);
  }, this);

  // Update existing
  _.each(update, function(id) {
    this.objects[id].updateState(state.objects[id]);
  }, this);

  // Remove missing
  _.each(remove, function(id) {
    this.removeObject(id);
  }, this);

  // -- nextObjectId
  this.nextObjectId = state.nextObjectId;

  // -- board
  this.board = state.board;

  // -- score
  var previousScore = this.score;
  this.score = state.score;
  if (!_.isEqual(previousScore, this.score)) {
    this.emit('score updated', this.score);
  }

  // -- time_step
  this.time_step = state.time_step;

  this.emit('state updated', this);
};

Game.prototype.resetGameState = function() {
  _.each(this.objects, function(object, id) {
    this.removeObject(id);
  }, this);
  this.nextObjectId = 0;
  this.score = 0;
  this.nextUpdate = this.startTime = Date.now();
  this.updates    = 0;
};

Game.prototype.updateObjectStates = function(states) {
  // Only update existing objects, ignore add/remove
  _.each(states, function(state) {
    if (state.id in this.objects) {
      this.objects[state.id].updateState(state);
    }
  }, this);  
};

//
// Generic object methods
//

Game.prototype.getObjects = function() {
  return _.values(this.objects);
};

Game.prototype.getObject = function(id) {
  return this.objects[id];
};

Game.prototype.addObject = function(state) {
  if (state.type === Player.TYPE) {
    return this._addPlayer(Player.createFromState(state));
  } else if (state.type === Ball.TYPE) {
    return this._addBall(Ball.createFromState(state));
  } else {
    return null;
  }
};

Game.prototype.removeObject = function(id) {
  if (!(id in this.objects)) return false;

  var object = this.objects[id]
    , method = 'remove' + object.type;

  if (!(method in this)) return false;
  
  return this[method](id);
};

//
// Player methods
//

Game.prototype.getPlayers = function() {
  return this._getObjectsOfType(Player.TYPE);
};

Game.prototype.getPlayer = function(id) {
  return this._getObjectOfType(id, Player.TYPE);
};

Game.prototype.addPlayer = function(name, attributes) {
  // Set attributes
  if (!attributes) attributes = {};
  attributes.name = name;

  return this._addPlayer(new Player(this._getNextObjectId(), attributes));
};

Game.prototype.removePlayer = function(id) {
  // console.log('Game.removePlayer(%s)', id);
  var player = this._removeObjectOfType(id, Player.TYPE);
  if (player) {
    this.emit('remove player', player);
  }
  return player;
};

//
// Ball methods
//

Game.prototype.getBalls = function() {
  return this._getObjectsOfType(Ball.TYPE);
};

Game.prototype.getBall = function(id) {
  return this._getObjectOfType(id, Ball.TYPE);
};

Game.prototype.addBall = function(attributes) {
  return this._addBall(new Ball(this._getNextObjectId(), attributes || {}));
};

Game.prototype.removeBall = function(id) {
  // console.log('Game.removeBall(%s)', id);
  var ball = this._removeObjectOfType(id, Ball.TYPE);
  if (ball) {
    ball.removeListener('dead', this._ballDead);
    this.emit('remove ball', ball);
  }
  return ball;
};

Game.prototype.getBallStates = function(type) {
  return _.map(this.getBalls(), function(object) { return object.getState(); });
};

//
// Tick method
//

Game.prototype.tick = function() {
  var self = this, i, j;

  while (this.nextUpdate < Date.now()) {
    // console.log('Step once using time: %s', this.time_step / 1000);
    var time = Date.now();

    // Update each object
    _.each(this.objects, function(object) {
      object.update(time, self);
    });

    // Check for collisions
    //  - http://fanitis.com/2011/03/15/collision-detector-performance-trick-1/
    var by_x = _.sortBy(this.objects, function(o) { return o.getPosition().x; });
    
    // console.log('checkCollisions for %d objects: %s', by_x.length
    //   , _.map(by_x, function(o) { return o.type + "[" + o.id + "]"; }));
    
    for (i=0; i<by_x.length - 1; i++) {
      for (j=i+1; j < by_x.length && by_x[j].getGeometry().left <= by_x[i].getGeometry().right; j++) {
        // console.log('checkCollisionWithObject(%d - %d)', i, j);
        if (by_x[i].checkCollisionWithObject(by_x[j])) {
          this.emit('objects collided', by_x[i], by_x[j]);
        }
      }
    }

    this.updates += 1;
    this.nextUpdate += this.time_step * 1000;

    // TODO: Add score for every 10 seconds with a live ball
    if (this.updates % Math.round(10 / this.time_step) === 0) {
      if (this._getObjectsOfType(Ball.TYPE).length > 0) {
        this.score += 1;
        this.emit('score updated', this.score);
      }
    }
  }

  // Update fps
  var elapsed = Date.now() - this.startTime;
  this.fps = (this.updates/elapsed*1000);

};

//
// Internal methods
//

Game.prototype._getObjectsOfType = function(type) {
  return _.filter(this.objects, function(object) { return object.type === type; });
};

Game.prototype._getObjectOfType = function(id, type) {
  var object = this.objects[id];
  if (object && object.id === id && object.type === type) {
    return object;
  } else {
    return null;
  }
};

Game.prototype._addObject = function(object) {
  // Make sure nextObjectId is still valid
  if (object.id >= this.nextObjectId) {
    this.nextObjectId = object.id + 1;
  }

  // Ignore existing check
  // // Check if object already exists
  // if (object.id in this.objects) throw new Error('Trying to add duplicate object: ' + object.id);

  // Add object and emit
  this.objects[object.id] = object;
  this.emit('add object', object);

  return object;
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

Game.prototype._addPlayer = function(player) {
  // console.log('Game.addPlayer(%s, "%s")', player.id, player.get('name'));

  // Add player to list of objects and emit
  this._addObject(player);
  this.emit('add player', player);

  return player;
};

Game.prototype._addBall = function(ball) {
  // console.log('Game.addBall(%s)', ball.id);

  // Add ball to list of objects
  this._addObject(ball);

  // Listen for dead events
  ball.on('dead', this._ballDead);

  // Emit add ball
  this.emit('add ball', ball);

  return ball;
};

Game.prototype._getNextObjectId = function() {
  var id = this.nextObjectId;
  this.nextObjectId += 1;
  return id;
};

Game.prototype._ballDead = function(ball, edge) {
  this.score -= 1;
  this.emit('dead ball', ball, edge, this.score);
  this.emit('score updated', this.score);
};