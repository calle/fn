(function(exports) {

  //
  // Scene
  //

  var Scene = exports.Scene = function(target, options) {
    if (typeof options !== 'object') options = {};

    // Setup renderer

    var SCREEN_WIDTH  = window.innerWidth;
    var SCREEN_HEIGHT = window.innerHeight;

    this.camera = new THREE.Camera( 75, SCREEN_WIDTH / SCREEN_HEIGHT, 1, 4000 );
    this.camera.position.z = 1000;

    this.scene = new THREE.Scene();

    this.renderer = new THREE.CanvasRenderer();
    this.renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);

    this.target = target;
    this.target.append(this.renderer.domElement);

    // Handle option

    this.debug = options.debug;

    this.fps = typeof options.fps === 'number' ? options.fps : 60;

    if (options.stats) {
      this.stats = new Stats();
      var statsElement = $(this.stats.domElement);
      statsElement.css({
        position: 'absolute', top: '0px', left: '0px'
      })
      this.target.append(statsElement);
    }

    if (options.controls) {
      this.controls = $('<div><input type="button" value="start" /><input type="button" value="stop" /></div>');
      this.controls.css({
        position: 'absolute', top: '0px', right: '0px'
      })
      this.controls.find('input[value="start"]').click(this.start.bind(this));
      this.controls.find('input[value="stop"]').click(this.stop.bind(this));
      this.target.append(this.controls);
    }

    // Setup internal state

    this.animation = new Animation(this.debug);
    this.interval = null;
    this.ticks = 0;
  };

  // Setting
  Scene.XRange = { min:-1250, max:1250 };
  Scene.YRange = { min:-800,  max:800  };
  Scene.ZRange = { min:-500, max:500  };

  Scene.prototype.start = function() {
    if (this.interval === null) {
      this.interval = setInterval(this.tick.bind(this), 1000 / 60);
    }
  };

  Scene.prototype.stop = function() {
    if (this.interval !== null) {
      clearInterval(this.interval);
      this.interval = null;
    }
  };

  Scene.prototype.tick = function() {
    try {
      // Step
      this.animation.update();
      if (this.stats) this.stats.update();

      // Render
      this.renderer.render(this.scene, this.camera);

      if (this.ticks++ % this.fps == 0) {
        console.log('animations: %d, objects: %d', this.animation.steps.length, this.scene.objects.length);
      }
    } catch (e) {
      console.error(e);
      console.error(e.stack);
      //this.stop();
    }
  };

  Scene.prototype.animate = function(target, name) {
    return this.animation.step(target, name);
  }

  Scene.prototype.setCameraPosition = function(x, y, z) {
    this.camera.position.x = x;
    this.camera.position.y = y;
    this.camera.position.z = z;
  }

  Scene.prototype.shiftCamera = function(x, y) {
    this.camera.position.x += (   x - this.camera.position.x ) * 0.05;
    this.camera.position.y += ( - y - this.camera.position.y ) * 0.05;
  }


  Scene.prototype.resetCamera = function() {
    this.animate(this.camera.position, 'resetCamera').
      to({ x:0, y:0 }, 500).
      start();
  }

  Scene.prototype.spin = function(spin) {
    if (spin && !this.spinning) {
      var self = this;
      this.spinning = self.animate({}).to({}, 5000).
        onUpdate(function(val) {
          self.setCameraPosition(
            Math.sin(val * 2 * Math.PI) * 1000,
            0,
            Math.cos(val * 2 * Math.PI) * 1000
            );
        });
      this.spinning.next(this.spinning);
      this.spinning.start();
    } else if (!spin && this.spinning) {
      this.spinning.removeNext(this.spinning);
      this.spinning = false;
    }
  }

  //
  // SceneObject
  //

  var SceneObject = exports.SceneObject = function(options) {
    // Return directly for inheritance case
    if (arguments.length === 0) return;

    this.options = options;
  };

  SceneObject.prototype.add = function(scene) {
    this.object = this._createObject();
    this.animation = this._createAnimation(scene, this.object);
    scene.scene.addObject(this.object);
    this.animation.start();
  };

  SceneObject.prototype.remove = function(scene) {
    if (this.animation) {
      this.animation.stop();
      delete this.animation;
    }
    if (this.object) {
      scene.scene.removeObject(this.object);
      delete this.object;
    }
  };

  SceneObject.prototype._createObject = function() {
  };

  SceneObject.prototype._createAnimation = function(scene, object) {
  };

  SceneObject.prototype.scale = function(factor) {
    this.object.scale.x = this.object.scale.y = factor;
  }

  //
  // Sprite
  //

  var Sprite = exports.Sprite = function(image_src, options) {
    // Return directly for inheritance case
    if (arguments.length === 0) return;

    SceneObject.call(this, options);

    this.image = new Image();
    this.texture = new THREE.Texture(null);
    this.image.onload = function () {
      console.log('setting texture.image to:')
      console.log(this.image)
      this.texture.image = this.image;
      this.texture.loaded = true;
      if (typeof options.onImageLoad === 'function') options.onImageLoad(this);
    }.bind(this);
    this.image.src = image_src;

    this.width  = options.width  || 300;
    this.height = options.height || 300;
  };

  Sprite.prototype = new SceneObject();
  Sprite.prototype.constructor = Sprite;

  Sprite.prototype._createObject = function() {
    // console.log('create sprite sized %dx%d', this.width, this.height);
    var material = new THREE.MeshBasicMaterial({ map:this.texture });
    var geometry = new Plane(this.width, this.height, 1, 1);
    var object = new THREE.Mesh(geometry, material);
    object.flipSided = true;
    object.doubleSided = true;
    object.scale.x = object.scale.y =  1;
    return object;
  };

  //
  // Particle
  //

  var Particle = exports.Particle = function(source, options) {
    // Return directly for inheritance case
    if (arguments.length === 0) return;

    SceneObject.call(this, options);

    if (source) {
      if (options.image) {
        console.log('Creating image particle')
        this.material = new THREE.ParticleBasicMaterial({ map:source });
      } else {
        console.log('Creating image for particle: %s', source)
        var image = new Image();
        jQuery(image).bind('load', function() {
          
          if (typeof options.onImageLoad === 'function') {
	           options.onImageLoad(this);
	        }
        });
        
        image.src = source;
        this.material = new THREE.ParticleBasicMaterial({ map:image });
      }
    } else {
      this.material = new THREE.ParticleCircleMaterial({ color: 0xffffff, opacity: 0.7 });
      this.objectScale = 5;
    }

    if (options.scale) {
      this.objectScale = options.scale;
    }
  };

  Particle.prototype = new SceneObject();
  Particle.prototype.constructor = Particle;

  Particle.prototype._createObject = function() {
    console.log('create particle');
    var object = new THREE.Particle(this.material);

    if (this.objectScale) {
      console.log('Setting Particle object scale to %s', this.objectScale);
      object.scale.x = object.scale.y = this.objectScale;
    }

    return object;
  };

})(window);
