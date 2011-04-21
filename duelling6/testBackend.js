var be = require('./lib/backend.js')();


be.query_backend(function(r){
	console.log(r);
});

