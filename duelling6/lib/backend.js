var sys = require('sys');
var exec = require('child_process').exec;

module.exports = function() {
	return new Backend();
}

Backend = function(){
}

var myurl = "http://profile.ak.fbcdn.net/hprofile-ak-snc4/23128_525938623_7208_n.jpg";

Backend.prototype.parse = function(data){
    var s = data.split("\n");
//    console.log(s);
    var question = s[0];
    var names = s.slice(1,-1);

    var result = {
	'question' : question,
	'names' : names
    };

    return result;
}

Backend.prototype.query_backend = function(callback){
    
	var self = this;
    exec("./cmd.sh", function(error, stdout, stderr){
//	     console.log(stdout);
	     var r = self.parse(stdout);
//	     console.log(r);
		callback(r);
	 });
}

