$(function(){

	var updatePhoto = function(photo) {
		if (photo.description) {
			$('#photo .description').text(photo.description);
			$('#photo img').attr("src", photo.data);
		} else {
			$('#photo .description').text("no photo");
			$('#photo img').attr("src", "");
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
