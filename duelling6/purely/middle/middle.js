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
    app.use(connect.staticProvider(__dirname + '/../front'));
    app.use(express.bodyDecoder());    // Needed for POST parameters
    app.set('views', __dirname + '/views');
});

app.get("/", function(req, res){
	    res.render("index.html");
});

app.get("/foo", function(req,res){
	    var questions = [];
	    be.query_backend(questions, function(data){
				 handle_backend_response(res,data);
			     });

//	    console.log(req);
	    var data = req.query;
	    var questions = data['questions'];
	    console.log(data);
 	    be.query_backend(questions,
		function(data){
		    send_response(res,data);
 		});
	    
	});

var handle_backend_response = function(res,data){
    if(data['result'] == 'QUESTION'){
	send_response(res,data);
    }
    else if(data['result'] == 'NO PERSON FOUND'){
	send_response(res,data);
    }
    else if(data['result'] == 'PERSON FOUND'){
	send_response(res, data);
    }
    else {
	console.log("Unexpected discriminator response: " + data['result']);
    }
};

app.post('/question',
	 function(req, res){
	     var data = req.body.data;
	     console.log("POST data: " + data);
	     var json = JSON.parse(data);
	     //	     console.log(json);
	     var questions = json['questions'];
	     //	     console.log(questions);

	     if(json['name']){
		 var name = json['name'];
		 be.learn(questions, name, function(data, msg){
			 if(data != -1){
			     console.log("Learned about: " + name);
			     var result = {'result': data};
			     send_response(res, result);
			 }
			 else{
			     var result = {'result': 'ERROR', 'msg' : msg};
			     send_response(res, result);
			 }
		     });

	     }
	     else{
		 be.query_backend(questions, function(data){
				      handle_backend_response(res,data);
				  });
	     }
	 });

var port = 8001;

app.listen(port);
console.log('Server running at http://localhost:' + port + '/');
