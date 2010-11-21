var snitch = function($) {
  var currentLists = {
    good: [],
    bad: []
  };
  
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
      avatar: 'https://assets2.yammer.com/user_uploaded/photos/p1/0124/5698/train_small.jpg'
    }, 
    {
      name: 'Tom Andersson',
      avatar: 'https://assets1.yammer.com/user_uploaded/photos/p1/0094/4546/catbagel_small.jpg'
    }, 
    {
      name: 'Magnus Wideberg',
      avatar: 'https://assets3.yammer.com/user_uploaded/photos/p1/0115/4413/T167228_Steve-McQueen-Posters_small.jpg'
    }, 
    {
      name: 'Ulf Liedberg',
      avatar: 'https://assets1.yammer.com/user_uploaded/photos/p1/0202/5809/uffe.boat_small.jpg'
    }, 
    {
      name: 'Mårten Sjöstrand',
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
        tags: [tags],
        inReplyTo: inReplyTo ? inReplyTo.name : null
      }
    };    
  }
  
  function generateMockLists() {
    var goodList = [];
    var badList = [];
    var name, goodName, badName, 
        seenGood  = {},
        seenBad   = {};
        
    for (var i = 0, n = 10; i < n; i++) {
      name = (mockUsers[Math.round(Math.random() * (mockUsers.length - 1))]).name;
      seenGood[name] =  (seenGood[name] >= 0) ? ++seenGood[name] : 0;
      seenBad[name] =  (seenBad[name] >= 0) ? ++seenBad[name] : 0;
      goodName = name + seenGood[name];
      badName = name + seenBad[name];
      goodList.push({
        name: goodName,
        points: Math.round(100 + Math.random() * 500)
      });

      badList.push({
        name: badName,
        points: Math.round(100 - Math.random() * 500)
      });
    } 
    
    return {
      score: {
        good: goodList,
        bad: badList
      }
    };
  }
  
  function appendMessage(message) {
    var message = message.message;
    var listNode = $('#thread-list');
    var time = new Date();
    
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
      '<li class="thread-list-item thread-replies-hidden yj-component first-message anim-hide">',
        '<div class="thread-starter yj-component">',
          '<div class="message-container">',
            '<div class="avatar yj-component">',
              '<a title="', message.from.name, '" href="#">',
                '<img class="avatar-thumb" src="', message.from.avatar, '">',
              '</a>',      
            '</div>',
            '<div class="message-body">',
              '<span class="byline">',
                '<a title="@', message.from.name, '" href="#">', message.from.name, '</a>', 
                (message.inReplyTo ? [' in reply to ', 
                  '<a title="@', message.inReplyTo, '" href="#">', message.inReplyTo, '</a>'].join('')
                   : ''), ': ',
              '</span>',
              message.content,
            '</div>',
            '<p class="attributes">',
              '<a class="message-time" title="', time.toString(), '" href="#">', time.toString(), '</a>',
            '</p>',
          '</div>',
        '</div>',
        '<div class="thread-replies-container">',
          '<div class="thread-replies-pointer"><!--wsb--></div>',
          '<div class="thread-topics yj-component">',
            '<div class="topics-container clearfix">',
              '<ul class="topics ">',
                tags.join(''),
              '</ul>',
            '</div>',
          '</div>',
        '</div>',
      '</li>'
    ].join(''));
    
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
  
  function _createListItem(data, id) {
    return [
      '<li class="fix-hover feed-navigator-list-item" id="', id, '">',       
        '<div class="feed-list-item-container">',
          '<a title="', data.name, '" href="#" class="nav-list-link">',
            '<span style="display: none;" class="badge">0</span>',
            data.name,
            '<span class="points">', data.points, "</span>",
          '</a>',
        '</div>',
      '</li>'
    ].join(''); 
  }

  function _createRankList(newList, oldList, classPrefix, colours) {
	  var listData = currentLists[oldList];
		
		currentLists[oldList] = [];
	
		var newItems = [];
		var id, currNode, diff, currUser, 
        newItems    = [],
				// Always update the lists on create, else only do so if we have new data
        hasChanged = listData.length === 0;
    for (var i = 0, n = newList.length; i < n; i++) {
      currUser = newList[i];
      prevData = listData[currUser.name];
      id = classPrefix + '-item-' + i; 
      
      if (prevData && i !== prevData.pos) {
				currNode = $('#' + prevData.elementId + ' a');
        currNode.animate({
          color: (i < prevData.pos) ? colours.climb : colours.fall
        }, 1000);

				diff = currUser.points - prevData.points;
				diff = diff > 0 ? '+' + diff : diff;
				currNode.addClass('emph')
				currNode.find('span.points').html('(' + diff + ')&nbsp;');
				hasChanged = true;
      } 
      newItems.push(_createListItem(currUser, id));
      
      currentLists[oldList][currUser.name] = {
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

    var newBolshevikList = _createRankList(goodList, 'good', 'bolsheviks', {climb: '#416FC3', fall: '#D60000' });
		var newMenshevikList = _createRankList(badList, 'bad', 'mensheviks', {climb: '#D60000', fall: '#416FC3' });

    var updateFunction = function() {
			if (newBolshevikList) {
				$('#bolsheviks-list').animate({
	        opacity: 0
	      }, 200, function() {
	        $(this).replaceWith(newBolshevikList);
	        newBolshevikList.animate({
	          opacity: 1
	        }, 400);
	      });
			}
      
      if (newMenshevikList) {
				$('#mensheviks-list').animate({
	        opacity: 0
	      }, 200, function() {
	        $(this).replaceWith(newMenshevikList);
	        newMenshevikList.animate({
	          opacity: 1
	        }, 400);
	      });
			};
    };
    
    setTimeout(updateFunction, 5000);
  }
  
  return {
    init: function() {
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
      } else {
	      // TODO: Attach event handlers to Socket.IO events
			}
    }
		
  };
}(jQuery);

$(document).ready(function() {
  snitch.init();
});

/**
<li data-thread-id="69862119" class="thread-list-item thread-replies-hidden yj-component first-message">
  <div data-message-id="69862119" id="thread-starter69862119" class="thread-starter yj-component">
    <div class="message-container">
      <div class="avatar yj-component">
        <a title="kristofferadwent" href="https://www.yammer.com/netlight.se/users/kristofferadwent">
          <img class="avatar-thumb" src="https://assets2.yammer.com/user_uploaded/photos/p1/0115/4447/Kristoffer_Awent_small.jpg">
        </a>      
        <a class="follow" title="follow kristofferadwent's messages" sender-id="1329192" href="javascript://" style="display: none;">follow user</a>
      </div>
      <div class="message-body">
        <span class="byline">
          <a title="@kristofferadwent" href="https://www.yammer.com/netlight.se/users/kristofferadwent">Kristoffer Adwent</a>: 
        </span>
        TiVo lanseras i Sverige <br>
        <a target="_blank" title="http://feber.se/pryl/art/193973/tivo_lanseras_i_sverige/?utm_source=feedburner&amp;utm_medium=feed&amp;utm_campaign=Feed%3A+prylfeber+%28Prylfeber%29" href="http://feber.se/pryl/art/193973/tivo_lanseras_i_sverige/?utm_source=feedburner&amp;utm_medium=feed&amp;utm_campaign=Feed%3A+prylfeber+%28Prylfeber%29">http://feber.se/pryl/art/193973/tivo_lanseras_i_sverige/…</a>
      </div>
      <div class="attachments">
        <div class="file-attachments"></div>
        <div class="image-attachments"></div>
        <div class="ymodule-attachments">
          <div id="ymodule-instance-281157" class="ymodule-instance">
            <div style="display: table; line-height: 1.2em;">
              <h5 style="font-weight: bold; margin-bottom: 2px;">
                <a rel="nofollow" target="_blank" href="http://feber.se/pryl/art/193973/tivo_lanseras_i_sverige/?utm_source=feedburner&amp;utm_medium=feed&amp;utm_campaign=Feed%3A+prylfeber+%28Prylfeber%29">
http://feber.se/pryl/art/193973/tivo_lanseras_i_sv…
                </a>
              </h5>
            </div>
            <div class="clear"></div>
          </div>
        </div>
      </div>
      <p class="attributes">
        <a class="message-time" title="9:45 PM - Nov 20" href="https://www.yammer.com/netlight.se#/Threads/show?threadId=69862119">15 hours ago</a>
      </p>
      <ul class="actions">
        <li class="message-action-list-item">&nbsp;· 
          <a title="reply to this message" class="reply message-action" href="javascript://">Reply</a>
        </li>
        <li class="message-action-list-item">&nbsp;· 
            <span class="like-action-wrapper">
            <a class="like message-action" title="like this message and share it with my followers" href="javascript://">Like</a>&nbsp;·
          </span> 
        </li>
        <li data-attach-menu="true" data-create-callback="buildMoreMenu" class="click-menu-trigger2 message-action-list-item">
          <a title="view more message options" href="javascript://">More</a>
        </li>
      </ul>
    </div>
  </div>
  <div class="thread-replies-container">
    <div class="thread-replies-pointer"><!--wsb--></div>
    <div class="thread-topics yj-component" style="display: none;">
      <a href="javascript://" class="edit">Edit Topics</a>
      <div class="topics-container clearfix">
        <ul class="topics "></ul>
        <div class="add-form">
          <input type="text" value="Add topic..." class="add-topic inactive" id="topics-inline1" autocomplete="off">
          <a href="javascript://" class="add-btn new-btn-alt new-small-btn">Add</a>
        </div>
      </div>
    </div>
    <p class="liked-by thread-starter-likes yj-component">
      <a href="javascript://" class="like-icon"></a>Liked by 
      <span class="liked-by-you" style="display: none;">you, </span>
      <a title="@andersfredriksson" href="https://www.yammer.com/netlight.se/users/andersfredriksson">Anders&nbsp;Fredriksson</a> and <a title="@connysandstrom" href="https://www.yammer.com/netlight.se/users/connysandstrom">Conny&nbsp;Sandström</a>.
    </p>
    <div data-older-reply-count="3" class="show-replies-container">
      <a href="javascript://" title="show replies" class="show-older-replies">Show 3 older replies »</a>
    </div>
    <ul class="thread-replies">
      <li data-message-id="69762954" id="thread-reply-list-item69762954" class="thread-reply-list-item yj-component">
        <div class="message-container">
          <div class="avatar yj-component">
            <a title="andersfredriksson" href="https://www.yammer.com/netlight.se/users/andersfredriksson">
              <img class="avatar-thumb" src="https://assets0.yammer.com/user_uploaded/photos/p1/0205/5164/bild_small.JPG">
            </a>
            <a class="follow" title="follow andersfredriksson's messages" sender-id="2834613" href="javascript://" style="display: none;">follow user</a>
          </div>
          <div class="message-body">
            <span class="byline reply-to-thread-starter">
              <a title="@andersfredriksson" href="https://www.yammer.com/netlight.se/users/andersfredriksson">Anders Fredriksson</a>
              <a class="in-reply-to" id="in-reply-to-69762954" href="https://www.yammer.com/netlight.se#/Threads/show?threadId=69726326"> in reply to </a>
              <a class="in-reply-to-user" title="@andreasekegren" href="https://www.yammer.com/netlight.se/users/andreasekegren">Andreas Ekegren</a>: 
            </span>Den var mycket bra!
          </div>
          <p class="attributes">
            <a class="message-time" title="2:51 PM - Nov 19" href="https://www.yammer.com/netlight.se#/Threads/show?threadId=69726326">1 day ago</a>
          </p>
          <ul class="actions">
            <li class="message-action-list-item">&nbsp;· 
              <a title="reply to this message" class="reply message-action" href="javascript://">Reply</a>
            </li>
            <li class="message-action-list-item">&nbsp;· 
              <span class="like-action-wrapper">
                <a class="like message-action" title="like this message and share it with my followers" href="javascript://">Like</a>&nbsp;·
              </span>
            </li>
            <li data-attach-menu="true" data-create-callback="buildMoreMenu" class="click-menu-trigger2 message-action-list-item">
              <a title="view more message options" href="javascript://">More</a>
            </li>
          </ul>
          <p class="liked-by thread-reply-list-item-likes yj-component" style="display: none;">
            <a href="javascript://" class="like-icon"></a>Liked by 
            <span class="liked-by-you">you</span>.
          </p>
        </div>
      </li>
    </ul>
    <div class="more-button-container yj-component" id="more-button-container" style="display: block;">
      <a href="javascript://" class="more-button" id="more-button">
        <span>More <span class="count"></span></span>
      </a>
    </div>
  </div>
</li>
*/