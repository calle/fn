jQuery(function($) {

  /**
   * Setup scene
   */

  var scene = new Scene($('#container'), { fps: 60, controls: false, stats:false });

  // Create snow
  var snow = new Snow(200);

  // Start scene
  scene.start();

  /**
   * Setup global instances
   */

  var backgroundSound = new Sound('sounds/whitechristmas.mp3', {
      loop:true,
      volume: 0.5,
      debug:true
    });

  var primes = new Primes();

  var currentSound = null;

  var searchImages = [];

  /**
   * Setup SocketIO client
   */

  var socket = new io.Socket();
  socket.connect();

  socket.on('connect', function() {
    console.log('connected');
    $('body').css({ 'border-top': '3px solid green' })
    setTimeout(function() {
      $('body').css({ 'border': 'none' })
    }, 1000)
  });

  socket.on('message', function(data) {
    console.debug('received message');
    console.debug(data);

    if (data.action) {
      console.info('received action: %s', data.action.type);

      if (data.action.type === 'fish') {
        var fish = new Fish('images/fish1-left.png', { width:70, height: 60 });
        var jaws = new Sound('sounds/jaws-music.mp3', { debug:true, autoplay:true });
        setTimeout( function() { backgroundSound.volume(0.2); }, 3000 );
        setTimeout( function() { fish.add(scene); }, 8000 );
        setTimeout( function() { backgroundSound.volume(0.4); }, 18000 );

      } else if (data.action.type === 'slow') {
        var slow1 = new Sound('sounds/slow1.mp3', { autoplay:true });
        var slow2 = new Sound('sounds/slow2.mp3');
        var passing = new Passing('images/larsa.png', { duration:5000 })
        setTimeout( function() { 
          passing.add(scene);
          passing.scale(1.5);
        }, 1000  );
        setTimeout( function() { slow2.play(); }, 4000  );

      } else if (data.action.type === 'lightbulb') {
        var lightbulb = new Lightbulb('images/lampa.png', 'images/lampa-light.png')
        lightbulb.add(scene);
        var starwars = new Sound('sounds/starwars-short.mp3', { autoplay:true });
        backgroundSound.volume(0.2);
        setTimeout( function() { backgroundSound.volume(0.4); }, 23000 );

      } else if (data.action.type === 'image') {
        var passing = new Passing(data.action.url, { duraiton:data.action.duration || 15000 })
        passing.add(scene);
        passing.scale(1.5);

      } else if (data.action.type === 'background') {
        $('#backdrop').attr('src', data.action.url);

      } else if (data.action.type === 'sound') {
        if (currentSound) currentSound.stop();
        if (data.action.value) {
          currentSound = new Sound(data.action.value, { autoplay:true });
        }

      } else if (data.action.type === 'snow') {
        if (data.action.value === 'on') {
          snow.add(scene);
        } else if (data.action.value === 'off') {
          snow.remove(scene);
        }

      } else if (data.action.type === 'primes') {
        if (data.action.value === 'on') {
          primes.add(scene);
        } else if (data.action.value === 'off') {
          primes.remove(scene);
        }

      } else if (data.action.type === 'spin') {
        if (data.action.value === 'on') {
          scene.spin(true);
        } else if (data.action.value === 'off') {
          scene.spin(false);
        }

      } else if (data.action.type === 'music') {
        if (data.action.value === 'on') {
          backgroundSound.play();
        } else if (data.action.value === 'off') {
          backgroundSound.pause();
        }

      } else if (data.action.type === 'search') {
        // Rewrite some search queries
        var query = data.action.query
          .replace(/\blucia\b/g, 'jul lucia');

        // Take first five words
        query = query.split(/\s+/).slice(0, 5).join(' ');

        Search.search(query, function(result) {
          if (result) {
            console.log('create Passing from search image');
            var obj = new Passing(result.url, {
              width:result.width,
              height:result.height,
              duraiton:10000,
              onImageLoad: function() {
                console.log('search image loaded');
                obj.add(scene);
                obj.scale(5);
                searchImages.push(obj);
              },
              onComplete: function() {
                var index = searchImages.indexOf(obj);
                if (index !== 1) {
                  searchImages.splice(index, 1);
                }
              }
            })
          }
        })

      } else if (data.action.type === 'remove-search') {
        searchImages.forEach(function(obj) {
          obj.remove(scene);
        });
        searchImages = [];

      } else {
        console.info('unknown action: %s', data.action.type);

      }

    } else {
      // unknown message
    }

  });

  socket.on('disconnect', function() {
    console.log('disconnected');
    $('body').css({
      'border-top': '5px solid red'
    })
  });

  /**
   * Setup mouse events
   */

  var mouseStartX = 0;
  var mouseStartY = 0;

  document.addEventListener('mousedown', function(event) {
    mouseStartX = event.clientX;
    mouseStartY = event.clientY;
    document.addEventListener('mousemove', moveCamera, false);
  });

  document.addEventListener('mouseup', function() {
    document.removeEventListener('mousemove', moveCamera);
    scene.resetCamera();
  });

  function moveCamera(event) {
    mouseX = event.clientX - mouseStartX;
    mouseY = event.clientY - mouseStartY;
    scene.shiftCamera(mouseX, mouseY);
  }

  // Start initial anim
  snow.add(scene);
  backgroundSound.play();
  $('#backdrop').attr('src', '/images/backdrop4.jpg');
});
