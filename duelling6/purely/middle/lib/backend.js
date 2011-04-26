var sys = require('sys');
var exec = require('child_process').exec;
var pg = require('pg');

module.exports = function() {
    return new Backend();
};

Backend = function(){
    var connectionString = "pg://nl:nl@localhost:5432/netlighters";
    var self = this;
    pg.connect(connectionString,
	       function(err, client) {
		   if(err) {
		       console.log("Error connecting to database: " + err);
		   }
		   else{
		       self.client = client;
		       console.log("Connected to database");
		   }
	       });
};

Backend.prototype.answer = function(question_id, nl_id, answer, callback){
    var sql =
	"INSERT INTO answers " +
	"(question_id, netlighter_id, content) " +
	"VALUES($1,$2,$3)";

    var values = [question_id, nl_id, answer];
    this.client.query(sql, values,
		     function(err, result){
			 if(err){
			     console.log("Error saving answer: " + err);
			 }
			 else{
			     console.log("Successfully saved answer");
			 }
			 callback(err, result);
		     });
};


var myurl = "http://profile.ak.fbcdn.net/hprofile-ak-snc4/23128_525938623_7208_n.jpg";

Backend.prototype.parse = function(data){
    var s = data.split("\n");
    var question = s[0];
    var names = s.slice(1,-1);

    var result = {
	'question' : question,
	'names' : names
    };

    return result;
};

Backend.prototype.query_backend = function(old_questions, callback){
    
    var pairs = [];
    for(var i in old_questions){
	var q = old_questions[i];
	for(k in q){
	    console.log(k);
	    var p = k + "=" + q[k];
	    pairs.push(p);
	}
    }
    var input = pairs.join(",");

    console.log("Input: " + input);

    var self = this;
    exec("backend/discriminator \"" + input + "\"", function(error, stdout, stderr){
	     console.log(stdout);
	     var json = JSON.parse(stdout);
//	     console.log(json);
	     callback(json);
	 });
};
