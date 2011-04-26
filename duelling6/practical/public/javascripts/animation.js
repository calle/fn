(function(exports) {

  //
  // Animation
  //

  var Animation = exports.Animation = function(debug) {
    this.debug = debug;
    this.steps = [];
  };

  Animation.prototype.step = function(target, name) {
    return new AnimationStep(this, target, name);
  };

  Animation.prototype.update = function() {
    var self = this,
        time = new Date().getTime(),
        remove = [];

    this.steps.forEach(function(step, i) {
      if (step.update(time) === false) {
        if (self.debug) console.log('Animation.update() %s is completed, will be removed', step.name);
        remove.push(step);
      }
    });

    remove.forEach(function(step) {
      self.remove(step);
    });
  };

  Animation.prototype.add = function(step) {
    if (this.debug) console.log('Animation.add(%s)', step.name);
    this.steps.push(step);
  };

  Animation.prototype.remove = function(step) {
    var i = this.steps.indexOf(step);
    if (i !== -1) {
      if (this.debug) console.log('Animation.remove(%s)', animation.name);
      this.steps.splice(i, 1);
      return true;
    } else {
      if (this.debug) console.log('Animation.remove(%s) not found', step.name);
      return false;
    }
  };

  // Animation steps

  var AnimationStep = function(animation, target, name) {
    AnimationStep.counter++;

    this.animation = animation;
    this.target = target;
    this.name = name || ('step-' + AnimationStep.counter);

    this.debug = this.animation.debug;

    this.values = {
        start: {},
        delta: {},
        end:   {}
    };

    this.duration = 1000;
    this.delayTime = 0;
    this.startTime = null;
    this.easingFunction = Easing.Linear.EaseNone;

    this.onUpdateCallback = null;
    this.onCompleteCallback = null;

    this.following = [];
    this.limited = [];

    this.state = 'stopped';
  };

  AnimationStep.counter = 1;

  AnimationStep.prototype.delay = function(amount) {
    this.delayTime = amount;
    return this;
  };

  AnimationStep.prototype.easing = function(easing) {
    this.easingFunction = easing;
    return this;
  };

  AnimationStep.prototype.onUpdate = function(callback) {
    this.onUpdateCallback = callback;
    return this;
  };

  AnimationStep.prototype.onComplete = function(callback) {
    this.onCompleteCallback = callback;
    return this;
  };

  AnimationStep.prototype.next = function(step) {
    if (this.following.indexOf(step) === -1) {
      this.following.push(step);
    }
    return this;
  };

  AnimationStep.prototype.removeNext = function(step) {
    var index = this.following.indexOf(step)
    if (index !== -1) {
      this.following.splice(index, 1);
    }
    return this;
  };

  AnimationStep.prototype.limit = function(step) {
    if (this.limited.indexOf(step) === -1) {
      this.limited.push(step);
    }
    return this;
  };

  AnimationStep.prototype.removeLimit = function(step) {
    var index = this.limited.indexOf(step)
    if (index !== -1) {
      this.limited.splice(index, 1);
    }
    return this;
  };

  AnimationStep.prototype.to = function(properties, duration) {
    var property;

    if (duration !== null) this.duration = duration;

    for (property in properties) {
      // This prevents the engine from interpolating null values
      if (this.target[property] === null) continue;

      // The current values are read when the tween starts;
      // here we only store the final desired values
      this.values.end[property] = properties[property];
    }
    return this;
  };

  AnimationStep.prototype.start = function(time) {
    if (this.debug) console.log('%s.start', this.name)
    if (typeof time !== 'number') time = new Date().getTime();

    this.startTime = time + this.delayTime;

    for (var property in this.values.end) {
      // Again, prevent dealing with null values
      if (this.target[property] === null) continue;

      this.values.start[property] = this.target[property];
      this.values.delta[property] = this.values.end[property] - this.target[property];
    }

    this.animation.add(this);

    this.state = 'running';

    return this;
  };

  AnimationStep.prototype.stop = function() {
    if (this.debug) console.log('%s.stop', this.name)

    if (this.state === 'stopped') return;

    var removed = this.animation.remove(this);

    // Always stop limited steps
    this.limited.forEach(function(step) { step.stop(); });

    // Stop following if this step is not running
    if (removed === false) {
      this.following.forEach(function(step) { step.stop(); });
    }

    this.state = 'stopped';

    return this;
  };

  AnimationStep.prototype.running = function() {
    return this.state === 'running';
  }

  AnimationStep.prototype.completed = function() {
    return this.state === 'completed';
  }

  AnimationStep.prototype.stopped = function() {
    return this.state === 'stopped';
  }

  AnimationStep.prototype.update = function(time) {
    var property, elapsed, value;

    if (time < this.startTime) {
      return true;
    }

    if (this.duration <= 0) {
      elapsed = 1
    } else {
      elapsed = (time - this.startTime) / this.duration;
      if (elapsed > 1) elapsed = 1;
    }

    value = this.easingFunction(elapsed);

    for (property in this.values.delta) {
      this.target[property] = this.values.start[property] + this.values.delta[property] * value;
    }

    if (this.onUpdateCallback !== undefined && typeof this.onUpdateCallback === 'function') {
      this.onUpdateCallback.call(this.target, value);
    }

    if (elapsed === 1) {
      if (this.debug) console.log('%s completed', this.name)

      this.state = 'completed';

      // Stop limited steps
      this.limited.forEach(function(step) { step.stop(); });

      // Invoke onCompleteCallback
      if (typeof this.onCompleteCallback === 'function') {
        this.onCompleteCallback.call(this.target);
      }

      // Start following steps
      this.following.forEach(function(step) { step.start(); });

      return false;
    }

    return true;
  };

  // Easing

  var Easing = Animation.Easing = {
    Linear: {
      EaseNone: function(k) {
        return k;
      }
    },
    Quadratic: {
      EaseIn: function(k) {
        return k * k;
      },
      EaseOut: function(k) {
        return - k * ( k - 2 );
      },
      EaseInOut: function(k) {
        if ( ( k *= 2 ) < 1 ) return 0.5 * k * k;
        return - 0.5 * ( --k * ( k - 2 ) - 1 );
      }
    },
    Cubic: {
      EaseIn: function(k) {
        return k * k * k;
      },
      EaseOut: function(k) {
        return --k * k * k + 1;
      },
      EaseInOut: function(k) {
        if ( ( k *= 2 ) < 1 ) return 0.5 * k * k * k;
        return 0.5 * ( ( k -= 2 ) * k * k + 2 );
      }
    },
    Quartic: {
      EaseIn: function(k) {
        return k * k * k * k;
      },
      EaseOut: function(k) {
         return - ( --k * k * k * k - 1 );
      },
      EaseInOut: function(k) {
        if ( ( k *= 2 ) < 1) return 0.5 * k * k * k * k;
        return - 0.5 * ( ( k -= 2 ) * k * k * k - 2 );
      }
    },
    Quintic: {
      EaseIn: function(k) {
        return k * k * k * k * k;
      },
      EaseOut: function(k) {
        return ( k = k - 1 ) * k * k * k * k + 1;
      },
      EaseInOut: function(k) {
        if ( ( k *= 2 ) < 1 ) return 0.5 * k * k * k * k * k;
        return 0.5 * ( ( k -= 2 ) * k * k * k * k + 2 );
      }
    },
    Sinusoidal: {
      EaseIn: function(k) {
        return - Math.cos( k * Math.PI / 2 ) + 1;
      },
      EaseOut: function(k) {
        return Math.sin( k * Math.PI / 2 );
      },
      EaseInOut: function(k) {
        return - 0.5 * ( Math.cos( Math.PI * k ) - 1 );
      }
    },
    Exponential: {
      EaseIn: function(k) {
        return k == 0 ? 0 : Math.pow( 2, 10 * ( k - 1 ) );
      },
      EaseOut: function(k) {
        return k == 1 ? 1 : - Math.pow( 2, - 10 * k ) + 1;
      },
      EaseInOut: function(k) {
        if ( k == 0 ) return 0;
        if ( k == 1 ) return 1;
        if ( ( k *= 2 ) < 1 ) return 0.5 * Math.pow( 2, 10 * ( k - 1 ) );
        return 0.5 * ( - Math.pow( 2, - 10 * ( k - 1 ) ) + 2 );
      }
    },
    Circular: {
      EaseIn: function(k) {
        return - ( Math.sqrt( 1 - k * k ) - 1);
      },
      EaseOut: function(k) {
        return Math.sqrt( 1 - --k * k );
      },
      EaseInOut: function(k) {
        if ( ( k /= 0.5 ) < 1) return - 0.5 * ( Math.sqrt( 1 - k * k) - 1);
        return 0.5 * ( Math.sqrt( 1 - ( k -= 2) * k) + 1);
      }
    },
    Elastic: {
      EaseIn: function(k) {
        var s, a = 0.1, p = 0.4;
        if ( k == 0 ) return 0; if ( k == 1 ) return 1; if ( !p ) p = 0.3;
        if ( !a || a < 1 ) { a = 1; s = p / 4; }
        else s = p / ( 2 * Math.PI ) * Math.asin( 1 / a );
        return - ( a * Math.pow( 2, 10 * ( k -= 1 ) ) * Math.sin( ( k - s ) * ( 2 * Math.PI ) / p ) );
      },
      EaseOut: function(k) {
        var s, a = 0.1, p = 0.4;
        if ( k == 0 ) return 0; if ( k == 1 ) return 1; if ( !p ) p = 0.3;
        if ( !a || a < 1 ) { a = 1; s = p / 4; }
        else s = p / ( 2 * Math.PI ) * Math.asin( 1 / a );
        return ( a * Math.pow( 2, - 10 * k) * Math.sin( ( k - s ) * ( 2 * Math.PI ) / p ) + 1 );
      },
      EaseInOut: function(k) {
        var s, a = 0.1, p = 0.4;
        if ( k == 0 ) return 0; if ( k == 1 ) return 1; if ( !p ) p = 0.3;
        if ( !a || a < 1 ) { a = 1; s = p / 4; }
        else s = p / ( 2 * Math.PI ) * Math.asin( 1 / a );
        if ( ( k *= 2 ) < 1 ) return - 0.5 * ( a * Math.pow( 2, 10 * ( k -= 1 ) ) * Math.sin( ( k - s ) * ( 2 * Math.PI ) / p ) );
        return a * Math.pow( 2, -10 * ( k -= 1 ) ) * Math.sin( ( k - s ) * ( 2 * Math.PI ) / p ) * 0.5 + 1;
      }
    },
    Back: {
      EaseIn: function(k) {
        var s = 1.70158;
        return k * k * ( ( s + 1 ) * k - s );
      },
      EaseOut: function(k) {
        var s = 1.70158;
        return ( k = k - 1 ) * k * ( ( s + 1 ) * k + s ) + 1;
      },
      EaseInOut: function(k) {
        var s = 1.70158 * 1.525;
        if ( ( k *= 2 ) < 1 ) return 0.5 * ( k * k * ( ( s + 1 ) * k - s ) );
        return 0.5 * ( ( k -= 2 ) * k * ( ( s + 1 ) * k + s ) + 2 );
      }
    },
    Bounce: {
      EaseIn: function(k) {
        return 1 - Easing.Bounce.EaseOut( 1 - k );
      },
      EaseOut: function(k) {
        if ( ( k /= 1 ) < ( 1 / 2.75 ) ) {
          return 7.5625 * k * k;
        } else if ( k < ( 2 / 2.75 ) ) {
          return 7.5625 * ( k -= ( 1.5 / 2.75 ) ) * k + 0.75;
        } else if ( k < ( 2.5 / 2.75 ) ) {
          return 7.5625 * ( k -= ( 2.25 / 2.75 ) ) * k + 0.9375;
        } else {
          return 7.5625 * ( k -= ( 2.625 / 2.75 ) ) * k + 0.984375;
        }
      },
      EaseInOut: function(k) {
        if ( k < 0.5 ) return Easing.Bounce.EaseIn( k * 2 ) * 0.5;
        return Easing.Bounce.EaseOut( k * 2 - 1 ) * 0.5 + 0.5;
      }
    }
  };

})(window);
