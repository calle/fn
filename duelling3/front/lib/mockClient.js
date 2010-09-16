var Battlefield = require('./battlefield');



var battlefield = new Battlefield('localhost', 3001);

var id = '344'

battlefield.login(id, 'calle', {
  login: function(err, resp) {
    console.log('successfull login');
    
    battlefield.move(id, 'east');
    battlefield.move(id, 'east');
    battlefield.move(id, 'east');
    battlefield.move(id, 'east');
    battlefield.move(id, 'east');

    battlefield.shoot(id, { x:3, y: 6 });

    battlefield.taunt(id, 'olle', 'your mama');

    battlefield.logout(id);
  },
  update: function(message) {
    console.log('got update: ' + message);
  },
  end: function() {
    console.log('client terminated');
  }
});


