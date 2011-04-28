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
    exec("backend/run_discriminator \"" + input + "\"", function(error, stdout, stderr){
	     console.log(stdout);
	     var json = JSON.parse(stdout);
//	     console.log(json);
	     callback(json);
	 });
};

Backend.prototype._save_answer = function(q_name, q_answer, name, callback){
    var sql = "insert into answers (question_id, netlighter_id, content) values( (select id from questions where name = $1), (select id from netlighters where name = $2), $3)"

    var values = [q_name, name, q_answer];
    this.client.query(sql, values,
				function(err, result){
				    if(err){
					var msg = "Error saving answer: " + err;
					console.log(msg);
					callback(-1, msg);
				    }
				    else{
					var msg = "Successfully saved new fact about " + name + ", " + q_name + "=" + q_answer;
					console.log(msg);
					callback(msg);
				    }
				});

};

Backend.prototype._answer_exists = function(question_name, question_answer, netlighter_name, callback){
  
    var sql = "select count(*) from answers where question_id = (select id from questions where name = $1) and netlighter_id = (select id from netlighters where name = $2)";

    var values = [question_name, netlighter_name];
    var self = this;
    this.client.query(sql, values,
 		     function(err, result){
 			 if(err){
 			     console.log("Error checking if answer exists: " + err);
			     console.log(err);
			     callback(-1);
 			 }
 			 else{
 			     console.log("Successfully retrieved answer");
			     if(result.rows[0].count == 0){
				 self._save_answer(question_name, question_answer, netlighter_name, callback);
			     }
			     else{
				 var msg = "Answer already in database for question: " + question_name + " for " + netlighter_name;
				 console.log(msg);
				 callback(-1, msg);
			     }
 			 }

 		     });
};

Backend.prototype.learn = function(questions, name, callback){
    // Only save the last question since we should have answers for the others
    var lastQuestion = questions[questions.length - 1];
    for(var k in lastQuestion){
	this._answer_exists(k, lastQuestion[k], name, callback);
	return;
    }
}
