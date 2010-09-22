jQuery(function($) {

  var post = function(url, data, callback, errback) {
    if (typeof(data) === 'function') {
      errback = callback;
      callback = data;
      data = null;
    }
    $.ajax({ 
      url: url, 
      type: "POST", 
      data: data, 
      dataType: 'json',
      error: errback || function() {},
      success: callback });
  }

  var rows = [],
      board, 
      position,
      direction,
      size,
      clients = [];

  $('#field input').hide();
  
  // Try getting existing connection
  post('/state', function(result) {
    console.log(result);

    // Update state
    board = new Board(result.board.width, result.board.height);
    position = result.position;
    direction = result.direction;
    size = result.size;
    clients = [result.name].concat(result.clients);
    
    setup();
  }, function(error) {
    $('#field input').show();
  })

  $('#info #logout').click(function() {
    post('/logout', function() {
      $('#field table').remove();
      $('#field input').show();
    });
  });

  // On login attempt
  $('#field input').keydown(function(event) {
    if (event.keyCode == '13') {
      event.preventDefault();
      
      var username = $(this).val();
      
      $(this).hide();
      
      post('/login', 'name=' + username, function(result) {
        console.log(result);

        // Update state
        board = new Board(result.board.width, result.board.height);
        position = result.position;
        direction = result.direction;
        size = result.size;
        clients = [username].concat(result.clients);
        
        setup();
      });
    }
  });

  var setup = function() {
    // Setup rows
    var row, td;

    for (i = 0; i < board.height; i++) {
      row = [];
      for (j = 0; j < board.width; j++) {
        row.push(setupTd(j, i));
      }
      rows.push(row);
    }
    
    // Setup table
    console.log('setup table with %d rows and %d columns', rows.length, board.width);
    var table = $('<table />');
    $.each(rows, function(y, row) {
      var tr = $('<tr />');
      $.each(row, function(x, td) {
        td.appendTo(tr);
      });
      tr.prependTo(table);
    });
    table.appendTo("#field");

    setupKeyListener();

    setTimeout(pollForUpdates, 200);

    // Update clients list
    redrawClients();
    
    // Draw table
    redrawTable();
  }

  var setupTd = function(x, y) {
    var td = $('<td />', { id:'board_' + x + '_' + y });
    td.click(function() {
      
      post('/fire', { x:x, y:y }, function(result) {
        console.log(result);
      })
      
      console.log('Shoot(%d,%d)', x, y);
    });
    return td;
  }

  var setupKeyListener = function() {
    $(document).keydown(function(event) {
      switch (event.keyCode) {
        case 38:
          return move('forward');
        case 40:
          return move('back');
        case 39: 
          return move('right');
        case 37:
          return move('left');
      }
      console.log(event.keyCode)
    });
  }

  var pollForUpdates = function() {
    post('/status', function(messages) {
      if (messages.length > 0) {
        console.log(messages)
        $.each(messages, function(i, message) {
          $('#info #messages').append('<li>' + message + '</li>');
        });
      }
      // Retry on success
      pollForUpdates();
    }, function() { 
      // Retry on error also
      pollForUpdates();
    });
  }

  var move = function(towards) {
    console.log('move towards %s', towards);
    post('/move', 'direction=' + towards, function(result) {
      console.log('move: %o', result);
      position = result.position;
      direction = result.direction;
      redrawTable();
    })
  }

  var redrawClients = function() {
    console.log('redrawClients(%j)', clients);
    $('#info #clients').empty();
    $.each(clients, function(i, client) {
      $('#info #clients').append('<li>' + client + '</li>');
    })
  }

  var redrawTable = function() {
    // Clear all td:s
    $.each(rows, function(y, row) {
      $.each(row, function(x, td) {
        td.removeClass();
      })
    });
    
    // Draw boat at position, direction and size
    console.log('walk ship at %o to %s', position, direction);
    board.reverseWalk(position, direction, size, function(x, y, axis) {
      rows[y][x].addClass('ship');
      if (x === position.x && y === position.y) rows[y][x].addClass('head');
    });
    
  }

});
