global._ = require('./public/javascripts/vendor/underscore');

var Game = require('./lib/game');

var game_attributes = {
  size      : { width: 10, height: 10 }
, time_step : 1/30
};
var ball_attributes = {
  position : { x: 2, y: 5 }
, size     : { width: 0.1, height: 0.1 }
, speed    : 4
};
var player_attributes = {
  position : { x: game_attributes.size.width - 0.1, y: 0 }
, size     : { width: 0.1, height: 1 }
, bursts   : { friction: 0.9, duration: 1/10, force: 30, reload: 1 }
};
var enemy_attributes = {
  position : { x: 0, y: 0 }
, size     : { width: player_attributes.size.width, height: 2 } // game_attributes.size.height }
, bursts   : player_attributes.bursts
};

// Create game and initial enemy
var game = new Game(game_attributes);

var addBall = function() {
  var ball = game.addBall(ball_attributes);
  ball.once('dead', function() {
    setTimeout(function() {
      game.removeBall(ball.id);
      addBall();
    }, 3000);
  });
};

// var enemy = game.addPlayer('enemy', enemy_attributes);

// Add players
var game_size   = game_attributes.size
  , player_size = player_attributes.size;
for (var i=0; i<200; i++) {
  player_attributes.position.x = Math.random() * (game_size.width - player_size.width);
  player_attributes.position.y = Math.random() * (game_size.height - player_size.height);
  game.addPlayer('Player' + i, player_attributes);
}

// // Add 50 balls
// for (var i=0; i<50; i++) {
//   addBall();
// }

for (i=0; i<100000; i++) {
  game.tick();
}

return;

var ticks = 0, start = Date.now();

// Trigger update of game according to time_step
setInterval(function() {
  game.tick();
  ticks++;
}, game_attributes.time_step * 1000);

setInterval(function() {
  console.log('tps = %s', (ticks / (Date.now() - start) * 1000).toFixed(2));
}, 1000);

