(function(exports) {

  var AngleToRad = Math.PI/180;
  //
  // Static
  //

  var Static = exports.Static = function(image, options) {
    if (typeof options !== 'object') options = {};
    Particle.call(this, image, options);
    this.x = typeof options.x === 'number' ? options.x : 200;
    this.y = typeof options.y === 'number' ? options.y : 300;
    this.z = typeof options.z === 'number' ? options.z : 500;
  };

  Static.prototype = new Particle();
  Static.prototype.constructor = Static;

  Static.prototype.moveTo = function(x, y, z, duration) {
    if (typeof x !== 'number') x = this.x;
    if (typeof y !== 'number') y = this.y;
    if (typeof z !== 'number') z = this.z;
    if (typeof duration !== 'number') duration = 0;

    this.scene.animate(this.object.position, 'move').
      to({ x:x, y:y, z:z }, 2000).
      start();
  };

  Static.prototype._createAnimation = function(scene, object) {
    this.scene = scene;
    this.object = object;

    return scene.animate(object.position, 'set').
      to({ x:this.x, y:this.y, z:this.z }, 0);
  };

  //
  // Passing
  //

  var Passing = exports.Passing = function(image, options) {
    if (typeof options !== 'object') options = {};
    Particle.call(this, image, options);
    this.duration = options.duration || 15000;
    this.onComplete = options.onComplete;
  };

  Passing.prototype = new Particle();
  Passing.prototype.constructor = Passing;

  Passing.prototype._createAnimation = function(scene, object) {
    var position = object.position;

    var reset = scene.animate(position, 'reset').
      to({
        x: Scene.XRange.max,
        y: (Scene.YRange.max + Scene.YRange.min) / 2 + (Math.random() * 1000 - 500),
        z: 0 + (Math.random() * 200 - 100)
      }, 0);

    var passAcross = scene.animate(position, 'passAcross').
      to({
        x: Scene.XRange.min
      }, this.duration);

    // Link
    reset.next(passAcross);
    passAcross.onComplete(function() {
      this.remove(scene);
      if (typeof this.onComplete === 'function') this.onComplete(this);
    }.bind(this));

    return reset;
  };

  //
  // Dropping
  //

  var Dropping = exports.Dropping = function(image, options) {
    if (typeof options !== 'object') options = {};
    Particle.call(this, image, options);
    this.duration = options.duration || 10000;
  };

  Dropping.prototype = new Particle();
  Dropping.prototype.constructor = Dropping;

  Dropping.prototype._createAnimation = function(scene, object) {
    var position = object.position;

    var reset = scene.animate(position, 'reset').
      to({
        x: (Scene.XRange.max + Scene.XRange.min) / 2 + (Math.random() * 1000 - 500),
        y: Scene.YRange.max,
        z: 200
      }, 0);

    var drop = scene.animate(position, 'passAcross').
      to({
        y: Scene.YRange.min
      }, this.duration);

    // Link
    reset.next(drop);
    drop.onComplete(this.remove.bind(this, scene));

    return reset;
  };


})(window);