var Game       = require('./game')
  , GameObject = require('./gameobject')
  , Ball       = require('./ball')
  , Player     = require('./player');

//
// Box2d code
//

var b2Vec2         = Box2D.Common.Math.b2Vec2
	,	b2BodyDef      = Box2D.Dynamics.b2BodyDef
	,	b2Body         = Box2D.Dynamics.b2Body
	,	b2FixtureDef   = Box2D.Dynamics.b2FixtureDef
	,	b2Fixture      = Box2D.Dynamics.b2Fixture
	,	b2World        = Box2D.Dynamics.b2World
	,	b2MassData     = Box2D.Collision.Shapes.b2MassData
	,	b2PolygonShape = Box2D.Collision.Shapes.b2PolygonShape;

// Extend without invoking base construcot
var extend = function(child, base) {
  var empty_constructor = function() {};
  empty_constructor.prototype = base.prototype;
  child.prototype = new empty_constructor();
};

//
// Game
//

var Box2dGame = module.exports = function() {
  Game.apply(this, arguments);

  this.world = new b2World(new b2Vec2(0, 0), true);

  // Create game boundaries
  var bodyDef, body;
  bodyDef            = new b2BodyDef;
  bodyDef.position.x = 0;
  bodyDef.position.y = 0;

  var fixDef         = new b2FixtureDef;
  fixDef.friction    = 0;
  fixDef.restitution = 1;

  // Don't detect collitions between walls and sensors
  fixDef.filter.groupIndex = -1;

  // Create staic walls for ball to bounce on
  bodyDef.type     = b2Body.b2_staticBody;
  bodyDef.userData = 'Wall';
  body             = this.world.CreateBody(bodyDef);
  this.staticWalls = _createSquareFixtures(body, fixDef, this.board);

  // Create sensor walls for players and balls to react to
  bodyDef.type     = b2Body.b2_dynamicBody;
  bodyDef.userData = 'WallSensor';
  body             = this.world.CreateBody(bodyDef);
  fixDef.isSensor  = true;
  this.sensorWalls = _createSquareFixtures(body, fixDef, this.board);

  // Add contact listener for walls
  var sensors = this.sensorWalls;

  var contactListener = new Box2D.Dynamics.b2ContactListener;
  contactListener.BeginContact = function(contact) {
    var a = contact.GetFixtureA(), b = contact.GetFixtureB()
      , sensor, ball, player;

    var find = function(check) { return check(a) ? check(a) : check(b); };

    console.log('= BeginContact (%s[%s] contact %s[%s])',
      a.GetBody().GetUserData(),
      JSON.stringify(_shapeVertices(a)),
      b.GetBody().GetUserData(),
      JSON.stringify(_shapeVertices(b)));

    // Extract sensor, ball and player
    sensor = find(function(f) { if (f.GetBody().GetUserData() === 'WallSensor') return f; });
    ball   = find(function(f) { if (f.GetBody().GetUserData() === 'Ball') return f.GetUserData(); });
    player = find(function(f) { if (f.GetBody().GetUserData() === 'Player') return f.GetUserData(); });

    // Check sensor
    if (!sensor) {
      console.log('= no wall sensor contact');
      return;
    }

    // Check ball
    if (ball) {
      if (sensor === sensors.left) {
        console.log('= ball hit left sensor');
        // ball.die('left');
      } else if (wall === sensors.right) {
        console.log('= ball hit right sensor');
        // ball.die('right');
      } else {
        console.log('= ball hit top/bottom sensor');
      }
      return;
    }

    if (player) {
      console.log('>>> Player in contact with sensor: %s',
        JSON.stringify(_shapeVertices(sensor)));

      player.setVelocity({ x: 0, y: 0 });
      // if (sensor === sensors.left) {
      //   console.log('= ball hit left sensor');
      //   // ball.die('left');
      // } else if (wall === sensors.right) {
      //   console.log('= ball hit right sensor');
      //   // ball.die('right');
      // } else {
      //   console.log('= ball hit top/bottom sensor');
      // }
      return;
    }

    console.log('= unknown contacts: %s <-> %s', a.GetBody().GetUserData(), b.GetBody().GetUserData());
  };
  this.world.SetContactListener(contactListener);

  // Listen for own events to handle Box2d creation/destruction
  var world  = this.world
    , bodies = this.bodies = {};
  this.on('add object', function(object) {
    var position = object.getPosition()
      , size     = object.getSize()
      , velocity = object.getVelocity();

    console.log('Create box2d object for %s[%d] using obj_pos=%s, obj_size=%s and obj_v=%s', object.type, object.id,
      JSON.stringify(position), JSON.stringify(size), JSON.stringify(velocity));

    // Create box2d object
    var bodyDef              = new b2BodyDef;
    bodyDef.type             = object.type === 'Player' ? b2Body.b2_kinematicBody : b2Body.b2_dynamicBody;
    bodyDef.userData         = object.type;
    bodyDef.fixedRotation    = true;
    bodyDef.position.x       = position.x + size.width / 2;
    bodyDef.position.y       = position.y + size.height / 2;
    bodyDef.linearVelocity.x = velocity.x;
    bodyDef.linearVelocity.y = velocity.y;

    var fixDef         = new b2FixtureDef;
    fixDef.density     = 1.0;
    fixDef.friction    = 0;
    fixDef.restitution = 1;
    fixDef.userData    = object;
    fixDef.shape       = new b2PolygonShape;
    fixDef.shape.SetAsBox(size.width / 2, size.height / 2);

    var body     = world.CreateBody(bodyDef);
    body.fixture = body.CreateFixture(fixDef);

    console.log('Created box2d body: %s',
      JSON.stringify(_shapeVertices(body.fixture)));
    console.log(' - %s[%d] body_pos=%s, body_v=%s', object.type, object.id,
      JSON.stringify(body.GetPosition()), JSON.stringify(body.GetLinearVelocity()));

    bodies[object.id] = body;

    object.on('move', function(object, position) {
      // console.log('%s[%d] moves to %s', object.type, object.id, JSON.stringify(position));
    });
    object.on('change velocity', function(object, position) {
      console.log('%s[%d] change obj_v to %s', object.type, object.id, JSON.stringify(position));
    });
  });

  this.on('remove object', function(object) {
    if (object.id in bodies) {
      world.DestroyBody(bodies[object.id]);
      delete bodies[object.id];
    }
  });
  
};
extend(Box2dGame, Game);

// Override tick to use box2d
Game.prototype.tick = function() {
  var bodies = this.bodies;

  // Early exit 
  if (this.nextUpdate > Date.now()) return;

  // Copy object properties to Box2d bodies
  _.each(this.objects, function(object, id) {
    if (id in bodies) {
      var body     = bodies[id]
        , position = object.getPosition()
        , size     = object.getSize()
        , velocity = object.getVelocity();

      // console.log('Update body for %s[%d] with obj_pos=%s and obj_v=%s', object.type, id,
      //   JSON.stringify(position), JSON.stringify(velocity));

      body.SetPosition({
        x : position.x + size.width / 2
      , y : position.y + size.height / 2
      });
      body.fixture.GetShape().SetAsBox(size.width / 2, size.height / 2);
      body.SetLinearVelocity(velocity);

      // console.log('Updated body: %s', JSON.stringify(_shapeVertices(body.fixture)));
      // console.log(' - %s[%d] body_pos=%s, body_v=%s', object.type, id,
      //   JSON.stringify(body.GetPosition()), JSON.stringify(body.GetLinearVelocity()));
    }
  });

  // Step world until nextUpdate is after current time
  // console.log('Game.tick before step world, updates = %d', this.updates);
  while ((this.nextUpdate - Date.now()) < 0) {
    // console.log('Step once using time: %s', this.time_step / 1000);
    this.world.Step(this.time_step, 1, 1);
    this.world.ClearForces();
    this.updates += 1;
    this.nextUpdate += this.time_step * 1000;

    // if (this.updates % 100 === 1) {
      // _.each(bodies, function(body, id) {
        // if (id in this.objects) {
        //   console.log(' - %s[%d] body_pos=%s, body_v=%s', this.objects[id].type, id,
        //     JSON.stringify(body.GetPosition()), JSON.stringify(body.GetLinearVelocity()));
        // }
      // }, this);
    // }
  }
  // console.log('Game.tick after step, updates = %d', this.updates);

  // _.each(bodies, function(body, id) {
  //   if (id in this.objects) {
  //     console.log(' - %s[%d] body_pos=%s, body_v=%s', this.objects[id].type, id,
  //       JSON.stringify(body.GetPosition()), JSON.stringify(body.GetLinearVelocity()));
  //   }
  // }, this);
  // 
  // Stats
  var elapsed = Date.now() - this.startTime;
  this.fps = (this.updates/elapsed*1000);

  // console.log('>>> Game.tick fps=%s', this.fps);

  // Update object properties from Box2d bodies
  _.each(this.objects, function(object, id) {
    if (id in bodies) {
      var body     = bodies[id]
        , position = body.GetPosition()
        , size     = object.getSize()
        , velocity = body.GetLinearVelocity();

      // console.log('Update %s[%d] with body_pos=%s and body_v=%s', object.type, id,
      //   JSON.stringify(position), JSON.stringify(velocity));

      // NOTE: Size never changes in Box2d models
      object.setPosition({
        x : position.x - size.width / 2
      , y : position.y - size.height / 2
      });
      object.setVelocity(velocity);

      // console.log('Updated object %s[%d] with obj_pos=%s and obj_v=%s', object.type, id,
      //   JSON.stringify(object.getPosition()), JSON.stringify(object.getVelocity()));
    }
  });
};

//
// Utilities
//

function _createSquareFixtures(body, fixDef, bounds) {
  var result = {};

  fixDef.shape = new b2PolygonShape;

  fixDef.shape.SetAsEdge({ x: bounds.left, y: bounds.top }, { x: bounds.left, y: bounds.bottom });
  result.left = body.CreateFixture(fixDef);

  fixDef.shape.SetAsEdge({ x: bounds.right, y: bounds.top }, { x: bounds.right, y: bounds.bottom });
  result.right = body.CreateFixture(fixDef);

  fixDef.shape.SetAsEdge({ x: bounds.left, y: bounds.top }, { x: bounds.right, y: bounds.top });
  result.top = body.CreateFixture(fixDef);

  fixDef.shape.SetAsEdge({ x: bounds.left, y: bounds.bottom }, { x: bounds.right, y: bounds.bottom });
  result.bottom = body.CreateFixture(fixDef);
};

function _shapeVertices(fixture) {
  var body = fixture.GetBody();
  return _.map(fixture.GetShape().GetVertices(), function(v) {
    return body.GetWorldPoint(v);
  });
};