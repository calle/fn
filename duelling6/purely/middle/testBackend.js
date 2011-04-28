var be = require('./lib/backend.js')();


//be.query_backend([], function(r){
//	console.log(r);
//});



var questions = {
    'haircolor': 'blue',
    'glasses' : 'NO'    
};

var name = "Magnus Wideberg";

be.learn(questions, name, function(data){
	     console.log("Learned stuff");
});