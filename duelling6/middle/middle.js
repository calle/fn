var sys = require('sys');
//var exec = require('child_process').exec;
var http = require('http');
var be = require('./lib/backend.js')();

var myurl = "http://profile.ak.fbcdn.net/hprofile-ak-snc4/23128_525938623_7208_n.jpg";

function create_json_response(data){
	var json_names =
		data.names.map(function(d){
			return {
				name: d,
				url: myurl
			}
	});

	var json = {'question' : data.question,
				'candidates' : json_names };

	return json;
}

var server = http.createServer(function (req, res) {

	be.query_backend(function(data){
		var json = create_json_response(data);
		res.writeHead(200, {'Content-Type': 'text/plain'});
		res.end(JSON.stringify(json));
	});

//		      var foo = {question : "haircolor",
//				 candidates: [
//				     {
//   					 name: "Magnus",
//					 url: "foo.com/url"
//				     },
//				     {
//					 name: "Moritz",
//					 url: "foo.bar/moritz"
//				     }
//				 ]};
		      
		  });

var host = "localhost";
var port = 8001;
server.listen(port, host);

console.log('Server running at http://' + host + ':' + port + '/');

