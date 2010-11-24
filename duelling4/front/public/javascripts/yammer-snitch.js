var snitch = function($) {
  var currentLists = {
    good: [],
    bad: []
  };

  var initLists;
  
  var developerMode = window.location.search.match("dev=true") !== null;
  
  var loremIpsum = ["Lorem ipsum dolor sit amet, consectetur adipisicing elit,", 
    " sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ",
    "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex",
    " ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit",
    " esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat ",
    "non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."].join('');
    
  var mockUsers = [
    {
      name: 'Calle Wester',
      username: 'calle-wester',
      avatar: 'https://assets2.yammer.com/user_uploaded/photos/p1/0124/5698/train_small.jpg'
    }, 
    {
      name: 'Tom Andersson',
      username: 'tom-andersson',
      avatar: 'https://assets1.yammer.com/user_uploaded/photos/p1/0094/4546/catbagel_small.jpg'
    }, 
    {
      name: 'Magnus Wideberg',
      username: 'magnus-wideberg',
      avatar: 'https://assets3.yammer.com/user_uploaded/photos/p1/0115/4413/T167228_Steve-McQueen-Posters_small.jpg'
    }, 
    {
      name: 'Ulf Liedberg',
      username: 'ulf-liedberg',
      avatar: 'https://assets1.yammer.com/user_uploaded/photos/p1/0202/5809/uffe.boat_small.jpg'
    }, 
    {
      name: 'Mårten Sjöstrand',
      username: 'marten-sjostrand',
      avatar: 'https://assets3.yammer.com/user_uploaded/photos/p1/0138/2718/bild_small.jpg'
    }];
  var mockTags = ['fnnl', 'elakt', 'mendurå', 'some-tag', 'blabla'];
  
  function generateMockMessage() {
    var message = loremIpsum.substring(0, Math.random() * loremIpsum.length);
    var user = mockUsers[Math.round(Math.random() * (mockUsers.length - 1))];
    var tags = mockTags[Math.round(Math.random() * (mockTags.length - 1))];
    var inReplyTo;
    
    if (Math.random() > 0.6) {
      inReplyTo = mockUsers[Math.round(Math.random() * (mockUsers.length - 1))];
    }

    return {
      message: {
        from: user,
        content: message,
        tags: Math.random() > 0.7 ? [] : [tags],
        inReplyTo: inReplyTo ? inReplyTo : null
      }
    };    
  }
  
  var mockStore = {good: [], bad: []};
  function generateMockLists() {
    var goodList = [];
    var badList = [];
    var name, user, goodUser, badUser 
        seenGood  = {},
        seenBad   = {};
        
    for (var i = 0, n = 10; i < n; i++) {
      user = (mockUsers[Math.round(Math.random() * (mockUsers.length - 1))]);
      
      name = user.name
      seenGood[name] =  (seenGood[name] >= 0) ? ++seenGood[name] : 0;
      seenBad[name] =  (seenBad[name] >= 0) ? ++seenBad[name] : 0;
      
      goodUser  = $.extend({}, user, {name: name + seenGood[name]});
      badUser   = $.extend({}, user, {name: name + seenBad[name]});

      goodList.push({
        user: goodUser,
        points: 100 + Math.random() * 500
      });

      badList.push({
        user: badUser,
        points: 100 - Math.random() * 500
      });
    }
 
    if (mockStore.good.length === 0) {
      mockStore.good = goodList;
      mockStore.bad  = badList;
    } else {
      mockStore.good = Math.random() > 0.9 ? goodList : mockStore.good;
      mockStore.bad = Math.random() > 0.3 ? badList : mockStore.bad;
    }
    
    return {
      score: {
        good: mockStore.good,
        bad: mockStore.bad
      }
    };
  }
  
  function appendMessage(message) {
    var message = message.message;
    var listNode = $('#thread-list');
    var time = new Date(message.time);
    
    var tags = [];
    var currTag;
    for (var i = 0, n = message.tags.length; i < n; i++) {
      currTag = message.tags[i];
      tags.push([
        '<li class="topic">',
          '<a href="#" title="', currTag, '" class="topic-link">', currTag, '</a>',
        '</li>'
      ].join(''));
    }
    
    var newItem = $([
      '<li class="thread-list-item thread-replies-hidden yj-component first-message anim-hide" message-id="', message.id, '">',
        '<div class="thread-starter yj-component">',
          '<div class="message-container">',
            '<div class="avatar yj-component">',
               '<img class="avatar-thumb" src="', message.from.avatar, '">',
            '</div>',
            '<div class="message-body">',
              '<span class="byline">',
                '<a target="_blank" title="@', message.from.username, '" href="https://www.yammer.com/netlight.se/users/', message.from.username, '">', message.from.name, '</a>', 
                (message.inReplyTo ? [' in reply to ', 
                  '<a target="_blank" title="@', message.inReplyTo.username, '" href="https://www.yammer.com/netlight.se/users/', message.inReplyTo.username, '">', message.inReplyTo.name, '</a>', ].join('')
                   : ''), ': ',
              '</span>',
              message.content,
            '</div>',
            '<p class="attributes">',
              '<a class="message-time" title="', time.toString(), '" href="#">', time.toString(), '</a>',
            '</p>',
          '</div>',
        '</div>',
        (tags.length > 0 ? [
        '<div class="thread-replies-container">',
          '<div class="thread-replies-pointer"><!--wsb--></div>',
          '<div class="thread-topics yj-component">',
            '<div class="topics-container clearfix">',
              '<ul class="topics ">',
                tags.join(''),
              '</ul>',
            '</div>',
          '</div>',
        '</div>'].join('') : ''),
      '</li>'
    ].join(''));

    $('#spinner-container').hide(); 

    listNode.prepend(newItem);
    newItem.siblings().first().removeClass('first-message');
    newItem.animate({
      opacity: 1
    }, 1000);
    
    var listChildren = listNode.find('li.thread-list-item');
    if (listChildren.length > 10) {
      $(listChildren[listChildren.length - 1]).remove();
    }
    
  }
  
  function _createListItem(data, id, colour) {
    return [
      '<li class="fix-hover feed-navigator-list-item" id="', id, '">',       
        '<div class="feed-list-item-container user-container">',
          '<img src="', data.user.avatar, '" class="user-thumb image" alt="', data.user.name, '">',
          '<div class="user ', colour ? 'changed' : '', '">',
            '<a title="', data.user.name, '" href="https://www.yammer.com/netlight.se/users/', data.user.username, '" class="nav-list-link" style="color: ', colour, '">',
              '<span class="name">', data.user.name, '</span><br />',
            '</a>',
            '<span class="points">', data.points ? parseFloat(data.points).toFixed(1) : 0, " points.</span>",
            '<span class="points-diff anim-hide"></span>',
          '</div>',
        '</div>',
      '</li>'
    ].join('');
  }

  function _createRankList(newList, oldList, classPrefix, colours) {
    var listData = currentLists[oldList];
    
    currentLists[oldList] = {};
  
    var newItems = [];
    var id, currNode, diff, currUser, changedColour,
        newItems    = [],
        // Always update the lists on create, else only do so if we have new data
        hasChanged = initLists || false;
    for (var i = 0, n = newList.length; i < n; i++) {
      currUser = newList[i];
      prevData = listData[currUser.user.name];
      id = classPrefix + '-item-' + i; 
      changedColour = undefined;
      
      if (prevData && i !== prevData.pos) {
        changedColour = (i < prevData.pos) ? colours.climb : colours.fall;
        currNode = $('#' + prevData.elementId);
        currNode.find('span.name').animate({
          color: changedColour
        }, 1000);
        currNode.find('span.points-diff').animate({
          opacity: 1
        }, 1000);

        diff = parseFloat(currUser.points - prevData.points).toFixed(1);
        currNode.find('span.points-diff').append(diff > 0 ? '+' + diff : diff);
        hasChanged = true;
      } 
      newItems.push(_createListItem(currUser, id, changedColour));
      
      currentLists[oldList][currUser.user.name] = {
        pos: i,
        points: currUser.points,
        elementId: id
      };
    }
    return hasChanged ? 
      $(['<ul class="feed-list hover-menu nav-list anim-hide" id="', classPrefix, '-list">',
        newItems.join(''),
      '</ul>'].join('')) : null;
  }

  function updateLists(message) {
    var goodList  = message.score.good;
    var badList   = message.score.bad;

		var goodColor = $('.bolshevik .nav-title').css('color');
		var badColor 	= $('.menshevik .nav-title').css('color');

    var newBolshevikList = _createRankList(goodList, 'good', 'bolsheviks', {climb: goodColor, fall: badColor });
    var newMenshevikList = _createRankList(badList, 'bad', 'mensheviks', {climb: badColor, fall: goodColor });

    initLists = false;
    
    var cleanupFunction = function() {
      var defaultColor = $('.user').not('.changed').find('a').css('color');
      var nodes = $('.user.changed a')
      nodes.animate({
        color: defaultColor
      }, 500);
    };
    
    var updateFunction = function() {
      if (newBolshevikList) {
        $('#bolsheviks-list').animate({
          opacity: 0
        }, 200, function() {
          $(this).replaceWith(newBolshevikList);
          newBolshevikList.animate({
            opacity: 1
          }, 400, function() {
            setTimeout(cleanupFunction, 1000);
          });
        });
      }
      
      if (newMenshevikList) {
        $('#mensheviks-list').animate({
          opacity: 0
        }, 200, function() {
          $(this).replaceWith(newMenshevikList);
          newMenshevikList.animate({
            opacity: 1
          }, 400, function() {
            setTimeout(cleanupFunction, 1000);
          });
        });
      };
    };
    
    setTimeout(updateFunction, 5000);
  }
  
  return {
    init: function() {
      // Ensure we update the user lists
      initLists = true;
      
      // Use mock data if in dev mode
      if (developerMode) {
        var timeoutFunction = function() {
          appendMessage(generateMockMessage());
          setTimeout(timeoutFunction,  Math.random() * 10000);
        };
        setTimeout(timeoutFunction, 1000);
        
        var listTimeoutFunction = function() {
          updateLists(generateMockLists());
          setTimeout(listTimeoutFunction,  10000);
        };
        setTimeout(listTimeoutFunction, 1000);

        // Hide login and display page
        $('#authentication').hide();
        $('#content').show();

      } else {
        var socket = new io.Socket();
        var form = $('#authentication form'),
            submit = $('input[type=submit]', form);

        form.submit(function() {
          if (socket.connected) {
            socket.send({ login:$('input[name=token]', form).val() });
            submit.attr('disabled', true);
          }
          return false;
        });

        submit.attr('disabled', true);
        socket.connect();

        socket.on('connect', function() {
          submit.attr('disabled', false);
        });

        socket.on('message', function(data) {
          if (data.login === 'ok') {
            $('#authentication').hide();
            $('#content').show();
          } else if (data.login) {
            $('input[name=token]', form).val('');
            submit.attr('disabled', false);
          } else if (data.message) {
            console.log('got message')
            appendMessage(data);
          } else if (data.score) {
            console.log('got score')
            updateLists(data);
          } else {
            // unknown message
          }
        });

        socket.on('disconnect', function() {
          $('#content').hide();
          $('#authentication').show();
          submit.attr('disabled', true);
        });
      }
    }
    
  };
}(jQuery);

$(document).ready(function() {
  snitch.init();
});
