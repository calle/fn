var EventEmitter = require('events').EventEmitter;

/**
 * GameObject has the following attributes
 *  - p {x, y} (position of top left corner)
 *  - width, height (size)
 *  - v {x, y} (current velocity)
 */
var GameObject = module.exports = function(id, type, attributes) {
  EventEmitter.apply(this, []);

  if (!type) {
    throw new Error('Cannot create GameObject without type');
  }

  // Bind utility functions so they can be used in iterations
  _.bindAll(this, '_overlapsPosition', '_overlapsObject');

  this.id   = id;
  this.type = type;

  // Set attributes
  if (!attributes) {
    this._attributes = {};
  } else {
    this._attributes = _.clone(attributes); 
  }

  // Make sure position and velocity exists
  if (!this._attributes.p) this._attributes.p = { x: attributes.x || 0, y: attributes.y || 0 };
  if (!this._attributes.v) this._attributes.v = { x: 0, y: 0 };
};

GameObject.prototype = new EventEmitter();

var empty_constructor = function() {};
empty_constructor.prototype = GameObject.prototype;

GameObject.extend = function(constructor) {  
  constructor.prototype = new empty_constructor();
};


GameObject.prototype.get = function(name) {
  return this._attributes[name];
};

GameObject.prototype.set = function(name, value) {
  this._attributes[name] = value;
};

GameObject.prototype.getPosition = function() {
  return this.get('p');
};

GameObject.prototype.setPosition = function(position_) {
  var position = this.getPosition();
  position.x = position_.x;
  position.y = position_.y;
};

GameObject.prototype.getSize = function() {
  return { width: this.get('width'), height: this.get('height') };
};

GameObject.prototype.setSize = function(size) {
  this.set('width', size.width);
  this.set('height', size.height);
};

GameObject.prototype.getGeometry = function() {
  var position = this.getPosition()
    , result = {
        left   : position.x
      , top    : position.y
      , width  : this.get('width')
      , height : this.get('height')
      };
  result.right  = result.left + result.width;
  result.bottom = result.top  + result.height;
  return result;
};

GameObject.prototype.getVelocity = function() {
  return this.get('v');
};

GameObject.prototype.setVelocity = function(velocity_) {
  var velocity = this.getVelocity();
  velocity.x = velocity_.x;
  velocity.y = velocity_.y;
};

GameObject.prototype.getState = function() {
  var state = _.clone(this._attributes);
  state.id = this.id;
  state.type = this.type;
  return state;
};

GameObject.prototype.updateState = function(state) {
  var geom = this.getGeometry();

  this.id = state.id;
  this.type = state.type;
  this._attributes = state;

  var geom_ = this.getGeometry();
  if (geom.left !== geom_.left || geom.right  !== geom_.right ||
      geom.top  !== geom_.top  || geom.bottom !== geom_.bottom) {
    // Emit move event
    this.emit('move', this, geom_);
  }

  this.emit('state updated', this);
};

GameObject.prototype.update = function(time, game) {
  if (this.lastUpdate) {
    var velocity = this.getVelocity()
      , duration = (time - this.lastUpdate) / 1000;
    
    if (velocity.x !== 0 || velocity.y !== 0) {
      this._updatePosition(velocity, duration, game);
    };
  }
  this.lastUpdate = time;
};

GameObject.prototype._updatePosition = function(velocity, duration, game) {
  var position = this.getPosition();

  // Update position
  position.x += velocity.x * duration;
  position.y += velocity.y * duration;

  // Invoke _validatePosition if it is defined
  if (this._validatePosition) {
    this._validatePosition(position, velocity, duration, game);
  }

  // Emit move event
  this.emit('move', this, this.getGeometry());
};

GameObject.prototype._overlapsPosition = function(other_position) {
  var geom = this.getGeometry();
  return  geom.left   <= other_position.x &&
          geom.right  >= other_position.x &&
          geom.top    <= other_position.y &&
          geom.bottom >= other_position.y;
};

GameObject.prototype._overlapsObject = function(other) {
  var geom = this.getGeometry()
    , other_geom = other.getGeometry();

  var result = 
    geom.left   <= other_geom.right &&
    geom.right  >= other_geom.left &&
    geom.top    <= other_geom.bottom &&
    geom.bottom >= other_geom.top;

  console.log('Chack overlaps for %s and %s => %s', 
    JSON.stringify(geom), JSON.stringify(other_geom), result);

  return result;

  return  geom.left   <= other_geom.right &&
          geom.right  >= other_geom.left &&
          geom.top    <= other_geom.bottom &&
          geom.bottom >= other_geom.top;
};
