(function(exports) {

  //
  // Sound
  //

  var Sound = exports.Sound = function(audio_src, options) {
    if (typeof options !== 'object') options = {};

    this.name = audio_src;
    this.source = audio_src;
    this.options = options;

    this.debug = options.debug;

    this._setupSound(options);
  };

  Sound.prototype.volume = function(value) {
    this.sound.volume = value;
  }

  Sound.prototype.play = function() {
    if (this.debug) console.log('playing %s', this.name)
    this.sound.play();
  }

  Sound.prototype.restart = function() {
    if (this.debug) console.log('restart %s', this.name)
    this._setupSound();
    this.sound.play();
  }

  Sound.prototype.pause = function() {
    if (this.debug) console.log('pausing %s', this.name)
    this.sound.pause();
  }

  Sound.prototype.stop = function() {
    if (this.debug) console.log('stopping %s', this.name)
    this.sound.pause();
  }

  Sound.prototype.reload = function() {
    if (this.debug) console.log('reload %s', this.name)
    this._setupSound();
  }

  Sound.prototype._setupSound = function() {
    var self = this,
        source = this.source,
        options = this.options;

    if (self.debug) console.log('loading sound: %s', self.name)
    self.sound = new Audio(source);

    if (options.volume > 0) self.volume(options.volume);

    self.sound.addEventListener('canplay', function() {
      if (self.debug) console.log('sound %s canplay', self.name);
      if (options.autoplay) self.play();
      if (options.onLoad) options.onLoad(self);
    }, true);

    if (options.onEnd && !options.loop) {
      self.sound.addEventListener('ended', function() {
        options.onEnd(self);
      }, true);
    }

    if (options.loop) {
      self.sound.addEventListener('ended', function() {
        if (self.debug) console.log('restarting %s', self.name);
        self._setupSound();
      }, true);
    }
  }

})(window);