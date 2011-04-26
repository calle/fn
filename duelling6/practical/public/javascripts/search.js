(function(exports) {

  var Search = exports.Search = {
  };

  Search.search = function(query, callback) {
    Search.getSearchInstance(function(search) {

      // Restrict to extra large images only
      // search.setRestriction(google.search.ImageSearch.RESTRICT_IMAGESIZE, google.search.ImageSearch.IMAGESIZE_MEDIUM);

      // Only search for clipart
      // search.setRestriction(google.search.ImageSearch.RESTRICT_IMAGETYPE, google.search.ImageSearch.IMAGETYPE_CLIPART);
      // google.search.ImageSearch.IMAGETYPE_LINEART
      // google.search.ImageSearch.IMAGETYPE_FACES
      // google.search.ImageSearch.IMAGETYPE_PHOTO

      search.setSearchCompleteCallback(this, function() {
        console.log('search.SearchCompleteCallback(%d results)', search.results.length);
        if (search.results.length <= 0) {
          callback(null);
        } else {
          var first = search.results[0];
          callback({
            name:   first.titleNoFormatting,
            url:    first.tbUrl, // first.unescapedUrl
            width:  first.tbWidth,
            height: first.tbHeight
          });
        }
      });

      // Perform search
      console.log('search.execute(%s)', query);
      search.execute(query);
    });
  }

  //
  // Load google search functionality
  //

  var google_loaded_queue = [];

  Search.getSearchInstance = function(callback) {
    google_loaded_queue.push(callback);
  }

  google.setOnLoadCallback(function() {
    console.log('google.search loaded');

    // Replace getSeach functionality
    Search.getSearchInstance = function(callback) {
      callback(new google.search.ImageSearch());
    }

    // Retry enqueued callbacks
    google_loaded_queue.forEach(Search.getSearchInstance);

    // Clear queue
    delete google_loaded_queue;
  });

  // Load Google image search
  console.log('loading google.search');
  google.load('search', '1');

})(window);
