jQuery(function($) {

  var users = $('#admin ul.users')

  users.find('a.block').click(function() {
    var link = $(this);
    $.post(this.href + '?user=' + link.attr('user'), function(result) {
      link.text(result.blocked ? 'unblock' : 'block');
    })
    return false;
  })
  users.find('a.adjustment').click(function() {
    var link = $(this);

    var result = prompt("Enter adjustment", this.adjustment);
    if (result !== null) {
      var points = parseInt(result, 10)
      if (!isNaN(points)) {
        $.post(this.href + '?user=' + link.attr('user') + '&points=' + points, function(result) {
          if (result.points) {
            link.text(result.points + ' points.');
          } else {
            link.text('no adjustment');
          }
        })
      }
    }

    return false;
  })

});