$(document).ready(function() {

  $('#game').show();

  $('#score .value').text('-3');

  $('#board').css('height', 472.2);

  $('#board').append('<div class="player colliding" id="player-2" style="top: 200px; left: 390px; width: 24px; height: 24px; border-top-left-radius: 12px 12px; border-top-right-radius: 12px 12px; border-bottom-right-radius: 12px 12px; border-bottom-left-radius: 12px 12px; "><span class="name" style="left: 28px; top: 2px; ">Sim_2_1</span></div>');

  $('#board').append('<div class="player colliding" id="player-3" style="top: 204px; left: 371px; width: 24px; height: 24px; border-top-left-radius: 12px 12px; border-top-right-radius: 12px 12px; border-bottom-right-radius: 12px 12px; border-bottom-left-radius: 12px 12px; "><span class="name" style="left: 28px; top: 2px; ">Sim_1_1</span></div>');

  $('#board').append('<div class="player self" id="player-30" style="top: 377px; left: 265px; width: 24px; height: 24px; border-top-left-radius: 12px 12px; border-top-right-radius: 12px 12px; border-bottom-right-radius: 12px 12px; border-bottom-left-radius: 12px 12px; "><span class="name" style="left: 28px; top: 2px; ">Calle</span></div>');

  $('#board').append('<div class="ball" id="ball-35" style="top: 448px; left: 304px; width: 8px; height: 8px; border-top-left-radius: 4px 4px; border-top-right-radius: 4px 4px; border-bottom-right-radius: 4px 4px; border-bottom-left-radius: 4px 4px; "></div>');

  $('#board').append('<div class="player" id="player-22" style="top: 80px; left: 71px; width: 24px; height: 24px; border-top-left-radius: 12px 12px; border-top-right-radius: 12px 12px; border-bottom-right-radius: 12px 12px; border-bottom-left-radius: 12px 12px; "><span class="name" style="left: 28px; top: 2px; ">Other player</span></div>');

  $('#board').append('<div class="player self colliding" id="player-1" style="top: 377px; left: 365px; width: 24px; height: 24px; border-top-left-radius: 12px 12px; border-top-right-radius: 12px 12px; border-bottom-right-radius: 12px 12px; border-bottom-left-radius: 12px 12px; "><span class="name" style="left: 28px; top: 2px; ">Calle</span></div>');

  $('#board').append('<div class="ball colliding" id="ball-35" style="top: 48px; left: 604px; width: 8px; height: 8px; border-top-left-radius: 4px 4px; border-top-right-radius: 4px 4px; border-bottom-right-radius: 4px 4px; border-bottom-left-radius: 4px 4px; "></div>');

  $('#board').append('<div class="ball dead" id="ball-35" style="top: 448px; left: 784px; width: 8px; height: 8px; border-top-left-radius: 4px 4px; border-top-right-radius: 4px 4px; border-bottom-right-radius: 4px 4px; border-bottom-left-radius: 4px 4px; "></div>');

  $('#chat .messages').append('<li class="message"><span class="info">28/11 13:53:30 - </span><span class="name">Sim_1: </span><span class="content">Message from simulator 1</span></li><li class="message"><span class="info">28/11 13:53:43 - </span><span class="name">Sim_2: </span><span class="content">Message from simulator 2</span></li><li class="message"><span class="info">28/11 13:53:58 - </span><span class="name">Sim_2: </span><span class="content">Message from simulator 2</span></li><li class="message"><span class="info">28/11 13:54:14 - </span><span class="name">Sim_2: </span><span class="content">Message from simulator 2</span></li><li class="message"><span class="info">28/11 13:54:27 - </span><span class="name">Sim_1: </span><span class="content">Message from simulator 1</span></li><li class="message"><span class="info">28/11 13:54:38 - </span><span class="name">Sim_1: </span><span class="content">Message from simulator 1</span></li><li class="message"><span class="info">28/11 13:55:08 - </span><span class="name">Sim_1_1: </span><span class="content">Message from simulator 1</span></li><li class="message"><span class="info">28/11 13:55:08 - </span><span class="name">Sim_2_1: </span><span class="content">Message from simulator 2</span></li><li class="message"><span class="info">28/11 13:55:36 - </span><span class="name">Sim_2_1: </span><span class="content">Message from simulator 2</span></li><li class="message"><span class="info">28/11 13:56:07 - </span><span class="name">Sim_1_1: </span><span class="content">Message from simulator 1</span></li><li class="message"><span class="info">28/11 13:56:25 - </span><span class="name">Sim_1_1: </span><span class="content">Message from simulator 1</span></li><li class="message"><span class="info">28/11 13:56:29 - </span><span class="name">Sim_2_1: </span><span class="content">Message from simulator 2</span></li>');

  setInterval(function() {
    // $('#board').toggleClass('lost');
    $('#ball-35').toggleClass('dead');

    $('#player-1').toggleClass('colliding');
    $('#player-2').toggleClass('colliding');
    $('#player-3').toggleClass('colliding');

  }, 1000);

  Notify($("#notify"));
  $.notify.info('testing notification');

});
