global._ = require('../public/javascripts/vendor/underscore');
var GameObject = require('../lib/gameobject')
  , Vector     = require('../lib/vector');


describe('GameObject', function(){
  describe('_movingTowards()', function(){
    var create = function(p, v) {
      return new GameObject(0, 'test', {
        position : p
      , velocity : v
      });
    };
    var object = create({ x:1, y:1 }, { x:1, y:1 });

    it('should return true when objects are moving towards each other', function(){
      var towards = create({ x:2, y:1 }, { x:-1, y:0 });
      object._movingTowards(towards).should.equal(true);
    });

    it('should return false when objects are moving away from each other', function(){
      var away = create({ x:2, y:1 }, { x:1, y:0 });
      object._movingTowards(away).should.equal(false);
    });

    it('should return true when object are moving towards each other', function() {
      var still = create({ x:2, y:1 }, { x:0, y:0 });
      object._movingTowards(still).should.equal(true);
    });

    it('should be correct for object to the right', function() {
      // Velocity of x must be < 1
      var other = create({ x:2, y:1 }, { x:-1, y:0 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:2, y:1 }, { x:0, y:4 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:2, y:1 }, { x:0, y:-4 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:2, y:1 }, { x:0.9999, y:0 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:2, y:1 }, { x:1.00001, y:0 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:2, y:1 }, { x:1.00001, y:1000 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:2, y:1 }, { x:1.00001, y:-1000 });
      object._movingTowards(other).should.equal(false);
    });

    it('should be correct for object to the left', function() {
      // Velocity of x must be > 1
      var other = create({ x:0, y:1 }, { x:3, y:0 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:0, y:1 }, { x:2, y:4 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:0, y:1 }, { x:2, y:-4 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:0, y:1 }, { x:1.00001, y:0 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:0, y:1 }, { x:0.9999, y:0 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:0, y:1 }, { x:0.9999, y:1000 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:0, y:1 }, { x:0.9999, y:-1000 });
      object._movingTowards(other).should.equal(false);
    });

    it('should be correct for object over', function() {
      // Velocity of y must be > 1
      var other = create({ x:1, y:0 }, { x:0, y:3 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:0 }, { x:-5, y:2 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:0 }, { x:5, y:2 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:0 }, { x:0, y:1.00001 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:0 }, { x:0, y:0.9999 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:1, y:0 }, { x:1000, y:0.9999 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:1, y:0 }, { x:-1000, y:0.9999 });
      object._movingTowards(other).should.equal(false);
    });

    it('should be correct for object under', function() {
      // Velocity of y must be < 1
      var other = create({ x:1, y:2 }, { x:0, y:-1 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:2 }, { x:-5, y:0 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:2 }, { x:5, y:0 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:2 }, { x:0, y:0.9999 });
      object._movingTowards(other).should.equal(true);
      other = create({ x:1, y:2 }, { x:0, y:1.00001 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:1, y:2 }, { x:1000, y:1.00001 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:1, y:2 }, { x:-1000, y:1.00001 });
      object._movingTowards(other).should.equal(false);
    });

    it('should be correct for objects at same position', function() {
      // Will never be true
      var other = create({ x:1, y:1 }, { x:1, y:1 });
      object._movingTowards(other).should.equal(false);
      other = create({ x:1, y:1 }, { x:-1, y:1 });
      object._movingTowards(other).should.equal(false);
    });

  });

  describe('_performCollision()', function() {
    var counter = 0
      , create = function(p, v, size, weight) {
          return new GameObject(counter+=1, 'test', {
            position : p
          , velocity : v
          , size     : { width:size.w, height:size.h }
          , weight   : weight
          });
        };

    it('should change x velocity in horizontal bounce', function() {
      var o1 = create({ x:1, y:1 }, { x:1,  y:0 }, { w:1, h: 1}, 10)
        , o2 = create({ x:2, y:1 }, { x:-1, y:0 }, { w:1, h: 1}, 1);
      o1._performCollision(o2);

      var v1 = o1.getVelocity()
        , v2 = o2.getVelocity();

      v1.x.should.be.below(1);
      v2.x.should.be.above(-1);

      v1.y.should.equal(0);
      v2.y.should.equal(0);

      // Move away from each other
      (v2.x - v1.x).should.be.above(0);

      // Difference should be in proportion to weight
      var diff1 = v1.x - 1
        , diff2 = v2.x - -1;
      diff1.should.be.below(diff2);
      var weighted_diff1 = Math.abs(o1.get('weight') * diff1)
        , weighted_diff2 = Math.abs(o2.get('weight') * diff2);
      // Rounding errors makes us need to look at rounded values
      weighted_diff1.toFixed(5).should.equal(weighted_diff2.toFixed(5));
    });

    it('should work when one object is still', function() {
      var o1 = create({ x:1, y:1 }, { x:0, y:0  }, { w:1, h: 1}, 10)
        , o2 = create({ x:1, y:2 }, { x:0, y:-1 }, { w:1, h: 1}, 1);

      o1._performCollision(o2);

      var v1 = o1.getVelocity()
        , v2 = o2.getVelocity();

      v1.x.should.be.below(1e-10);
      v2.x.should.be.below(1e-10);

      v1.y.should.be.below(0);
      v2.y.should.be.above(-1);

      // Move away from each other
      (v2.y - v1.y).should.be.above(0);
    });

    it('should prevent object from overlapping', function() {
      var o1 = create({ x:1, y:6 }, { x:1,  y:1 }, { w:4, h:4 }, 10)
        , o2 = create({ x:5, y:7 }, { x:-1, y:0 }, { w:2, h:2 }, 1)
        , o3 = create({ x:4, y:8 }, { x:-1, y:0 }, { w:4, h:4 }, 1);

      // Should not move non-overlapping objects
      debugger
      o1._performCollision(o2);
      o1.getPosition().should.eql({ x:1, y:6 });
      o2.getPosition().should.eql({ x:5, y:7 });

      // Reset velocity of o1
      o1.setVelocity({ x:1, y:1 });

      // Should move overlapping objects to prevent overlapping
      o1._performCollision(o3);
      var p1 = o1.getPosition()
        , p2 = o3.getPosition();
      p1.x.should.be.below(1);
      p1.y.should.be.below(6);
      p2.x.should.be.above(4);
      p2.y.should.be.above(8);
    });

  });
});
