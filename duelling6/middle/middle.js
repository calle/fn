var sys = require('sys');
var http = require('http');
var express = require('express');
var connect = require('connect');
var be = require('./lib/backend.js')();

var myurl = "http://profile.ak.fbcdn.net/hprofile-ak-snc4/23128_525938623_7208_n.jpg";

function create_json_response(data){
	var json_names =
		data.names.map(function(d){
			return {
				name: d,
				url: myurl
			};
	});

	var json = {'question' : data.question,
				'candidates' : json_names };

	return json;
}

var app = express.createServer();

var send_response = function(res, data){
//    var json = create_json_response(data);
    res.writeHead(200, {'Content-Type': 'application/json'});
    res.end(JSON.stringify(data));
};

app.configure(function(){
    app.use(connect.compiler({ src: __dirname + '/public', enable: ['less'] }));
    app.use(connect.staticProvider(__dirname + '/public'));
    app.use(express.bodyDecoder());    // Needed for POST parameters
    app.set('views', __dirname + '/views');
});


app.get("/question", function(req,res){
 	    be.query_backend(
		function(data){
		    send_response(res,data);
 		});
	    
	});

app.post('/answer',
	 function(req, res){
	     var question_id = req.body.question_id;
	     var netlighters_id = req.body.netlighters_id;
	     var answer = req.body.answer;
	     be.answer(question_id, netlighters_id, answer,
		       function(){
			   
 			   be.query_backend(
			       function(data){
				   if(data['result'] == 'QUESTION'){
				       send_response(res,data);
				   }
				   else{
				       console.log("Unexpected discriminator response: " + data['result']);
				   }
 			       });
		       });
	 });

var port = 8001;

app.listen(port);
console.log('Server running at http://localhost:' + port + '/');
