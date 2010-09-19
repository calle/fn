// Directions mapping
var directions = {
  north: { axis: 'y', size: 'height', step:  1 },
  south: { axis: 'y', size: 'height', step: -1 },
  east:  { axis: 'x', size: 'width',  step:  1 },
  west:  { axis: 'x', size: 'width',  step: -1 }
};

// Constructor
var Board = module.exports = function(width, height) {
  if (!(this instanceof Board)) return new Board(size);

  // Default values
  if (typeof width !== 'number')  width  = 8;
  if (typeof height !== 'number') height = width;

  // Setup board size
  this.width = width;
  this.height = height;
};

Board.STOP = {};

Board.prototype.step = function(position, direction, steps) {
  var info = directions[direction],
      size = this[info.size],
      next = { x:position.x, y:position.y };

  // Validate input
  steps = (typeof steps === 'number' ? steps : 1);

  // Move to correct new value with wrapping over edges
  next[info.axis] = (next[info.axis] + (steps * info.step) + size) % size;

  // Return value
  return next;
};

Board.prototype.walk = function(start, direction, steps, callback) {
  var current = start,
      axis = directions[direction].axis;

  // Walk the number of steps and invoke callback each step
  for (var i = 0; i < steps; i++) {
    if (callback(current.x, current.y, axis) === Board.STOP) return;
    current = this.step(current, direction);
  }
};

Board.prototype.reverseWalk = function(end, direction, steps, callback) {
  var current = end,
      axis = directions[direction].axis;
  
  // Start by rolling back to start
  current = this.step(current, direction, -steps);

  // Walk forward again and invoke callback each step
  for (var i = 0; i < steps; i++) {
    current = this.step(current, direction);
    if (callback(current.x, current.y, axis) === Board.STOP) return;
  }
};