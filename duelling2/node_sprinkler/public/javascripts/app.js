$(function(){

	var updatePhoto = function(photo) {
		if (photo.data) {
			$('#photo .description').text(photo.description);
			$('#photo img').attr("src", photo.data);
			$('#photo img').show();
		}
	};
	
	(function poll() {
		$.ajax({
	    url: '/photo/next',
			dataType: 'json',
			success: updatePhoto,
      complete: poll
		});
	})()

  $('#photo .description').text("Wait for first photo...")

})
