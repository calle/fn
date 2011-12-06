var EventEmitter = require('events').EventEmitter
  , Vector       = require('./vector');

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

  // Clone attributes to preserve them
  attributes = attributes ? _.clone(attributes) : {};

  this.id   = id;
  this.type = type;

  // Set position, size, velocity and weight and remove from attributes
  this.position = new Vector(attributes.position || { x: 0, y: 0 });
  delete attributes.position;
  this.size     = _.clone(attributes.size) || { width: 1, height: 1 };
  delete attributes.size;
  this.velocity = new Vector(attributes.velocity || { x: 0, y: 0 });
  delete attributes.velocity;
  this.weight   = 'weight' in attributes ? attributes.weight : 1;
  delete attributes.weight;

  // Set remaining attributes
  this._attributes = attributes;
};

GameObject.prototype = new EventEmitter();

var empty_constructor = function() {};
empty_constructor.prototype = GameObject.prototype;
GameObject.extend = function(constructor) {
  constructor.prototype = new empty_constructor();
  constructor.createFromState = _.bind(GameObject.createFromState, null, constructor);
};

GameObject.createFromState = function(constructor, state) {
  var object = new constructor(state.id, {});
  object.updateState(state);
  return object;
};

GameObject.prototype.get = function(name) {
  return this._attributes[name];
};

GameObject.prototype.set = function(name, value) {
  this._attributes[name] = value;
};

GameObject.prototype.getPosition = function() {
  return this.position;
};

GameObject.prototype.setPosition = function(position_) {
  var position = this.getPosition();
  if (position.x !== position_.x || position.y !== position_.y) {
    position.x = position_.x;
    position.y = position_.y;
    this.emit('move', this, position);
  }
};

GameObject.prototype.getSize = function() {
  return this.size;
};

GameObject.prototype.setSize = function(size_) {
  var size = this.getSize();
  if (size.width !== size_.width || size.height !== size_.height) {
    size.width = size_.width;
    size.height = size_.height;
    this.emit('resize', this, size);
  }
};

GameObject.prototype.getVelocity = function() {
  return this.velocity;
};

GameObject.prototype.setVelocity = function(velocity_) {
  var velocity = this.getVelocity();
  if (velocity.x !== velocity_.x || velocity.y !== velocity_.y) {
    velocity.x = velocity_.x;
    velocity.y = velocity_.y;
    this.emit('change velocity', this, velocity);
  }
};

GameObject.prototype.getWeight = function() {
  return this.weight;
};

GameObject.prototype.setWeight = function(weight) {
  if (this.weight !== weight) {
    this.weight = weight;
    this.emit('change weight', this, weight);
  }
};

GameObject.prototype.getGeometry = function() {
  var position = this.getPosition()
    , size     = this.getSize()
    , result = {
        left   : position.x
      , top    : position.y
      , width  : size.width
      , height : size.height
      };
  result.right  = result.left + result.width;
  result.bottom = result.top  + result.height;
  return result;
};

GameObject.prototype.getState = function() {
  var state  = {};
  state.id   = this.id;
  state.type = this.type;

  state.position = this.getPosition();
  state.size     = this.getSize();
  state.velocity = this.getVelocity();
  state.weight   = this.getWeight();

  state.attributes = this._attributes;
  return state;
};

GameObject.prototype.updateState = function(state) {
  this.id   = state.id;
  this.type = state.type;

  this.setPosition(state.position);
  this.setSize(state.size);
  this.setVelocity(state.velocity);
  this.setWeight(state.weight);

  // Update remaining attribues
  this._attributes = state.attributes;

  // Finally emit state updated
  this.emit('state updated', this);
};

GameObject.prototype.update = function(time, game) {
  if (this.lastUpdate) {
    var position = _.clone(this.getPosition())
      , velocity = _.clone(this.getVelocity())
      , duration = (time - this.lastUpdate) / 1000;

    this._updateObject(position, velocity, duration, game);

    // Update position and velocity possibly emitting move/change velocity
    this.setPosition(position);
    this.setVelocity(velocity);
  }
  this.lastUpdate = time;
};

GameObject.prototype.checkCollisionWithObject = function(other) {
  // Make sure collision is valid
  if (!this._validCollisionTarget(other)) return false;
  if (!other._validCollisionTarget(this)) return false;
  // Check overlap and movement
  if (this._overlapsObject(other) && this._movingTowards(other)) {
    // Update object according to collision
    this._performCollision(other);
    return true;
  } else {
    return false;
  }
};

GameObject.prototype._updateObject = function(position, velocity, duration, game) {
  // Update position
  position.x += velocity.x * duration;
  position.y += velocity.y * duration;
};

GameObject.prototype._validCollisionTarget = function(other) {
  return true;
};

GameObject.prototype._overlapsObject = function(other) {
  // var p1 = this.getPosition()
  //   , s1 = this.getSize()
  //   , p2 = other.getPosition()
  //   , s2 = other.getSize();
  //
  // // Move positions to center of objects
  // p1 = new Vector({ x: p1.x + s1.width / 2, y: p1.y + s1.height / 2 });
  // p2 = new Vector({ x: p2.x + s2.width / 2, y: p2.y + s2.height / 2 });
  //
  // // Calculate distance vector
  // var d = p2.sub(p1);
  //
  // // Overlaps if distance is less than r1 + r2
  // return d.norm() <= (s1.width / 2 + s2.width / 2);
  var geom       = this.getGeometry()
    , other_geom = other.getGeometry();

  return  geom.left   <= other_geom.right &&
          geom.right  >= other_geom.left &&
          geom.top    <= other_geom.bottom &&
          geom.bottom >= other_geom.top;
};

GameObject.prototype._movingTowards = function(other) {
  /**
   * Idea is to project the difference in speed onto the line that
   * connects the centre of the two objects. If this projection is
   * negative the objects are moving towards each other.
   */
  var p1 = this.getPosition()
    , v1 = this.getVelocity()
    , p2 = other.getPosition()
    , v2 = other.getVelocity();

  var d  = p2.sub(p1)
    , dv = v2.sub(v1);
  return d.dotProd(dv) < 0;
};

GameObject.prototype._performCollision = function(other) {
  var p1 = this.getPosition()
    , v1 = this.getVelocity()
    , s1 = this.getSize()
    , m1 = this.getWeight()
    , p2 = other.getPosition()
    , v2 = other.getVelocity()
    , s2 = other.getSize()
    , m2 = other.getWeight();

  // Adjust position to be centered in each object and wrap in Vector
  p1 = new Vector({ x: p1.x + s1.width / 2, y: p1.y + s1.height / 2 });
  p2 = new Vector({ x: p2.x + s2.width / 2, y: p2.y + s2.height / 2 });

  var px = p1.x - p2.x;
  var py = p1.y - p2.y;
  var vx = v1.x - v2.x;
  var vy = v1.y - v2.y;
  var R  = (s1.width + s2.width) / 2; // Assume width == height of both objects

  // Find the exact time of the collision
  var p = - (2 * (px*vx + py*vy)/(vx*vx + vy*vy) );
  var q = (px*px + py*py - R*R)/(vx*vx + vy*vy);
  var tao = - p/2 + Math.sqrt( (p*p/4) - q);
  if (isNaN(tao) || tao < 0){
    tao = 0;
  }

  // Update placements with the true position at collision
  p1 = p1.sub(v1.scalarMulti(tao)); // p1 = p1 - tao * v1
  p2 = p2.sub(v2.scalarMulti(tao)); // p2 = p2 - tao * v2

  // Calculate distance and angle between objects
  var d   = p2.sub(p1)
    , phi = -d.angleRad();

  // Rotate basis phi degrees using the matrix:
  //  [cos(phi) -sin(phi); sin(phi)  cos(phi)] * [v.x, v.y]'
  //  o*V*Prim are all pre collision speeds in the new basis.
  var vPrim1 = v1.rotateRad(phi);
  var vPrim2 = v2.rotateRad(phi);

  // Calculate the corresponding values after collision:
  //  u1_x* = (v1_x*(m1 - m2) + 2m2 * v2_x* ) / (m1 + m2)
  //  u2_x* = (v2_x*(m2 - m1) + 2m1 * v1_x* ) / (m1 + m2)
  //  o1.m is the mass [kg] of the object.
  var uPrim1 = new Vector((vPrim1.x * (m1 - m2) + 2*m2 * vPrim2.x) / (m1 + m2), vPrim1.y);
  var uPrim2 = new Vector((vPrim2.x * (m2 - m1) + 2*m1 * vPrim1.x) / (m1 + m2), vPrim2.y);

  // New speed for o1 is [o1UxPrim, o1VyPrim] and for o2 [o2UxPrim, o2VyPrim]
  //  all the are expressed using the new basis.
  // Transform these into the regular basis by applying the inverse rotation.
  //  Rinv(phi) = [cos(phi) sin(phi); -sin(phi) cos(phi)]
  var u1 = uPrim1.rotateRad(-phi)
    , u2 = uPrim2.rotateRad(-phi);

  // Estimate the position tao seconds after the collision.
  p1 = p1.add(u1.scalarMulti(tao)); // p1 = p1 + tao * v1_
  p2 = p2.add(u2.scalarMulti(tao)); // p2 = p2 + tao * v2_

  // Adjust position to be top left again
  p1 = { x: p1.x - s1.width / 2, y: p1.y - s1.height / 2 };
  p2 = { x: p2.x - s2.width / 2, y: p2.y - s2.height / 2 };

  // Update position and velocities on both of the objects
  this.setPosition(p1);
  this.setVelocity(u1);
  other.setPosition(p2);
  other.setVelocity(u2);
};
