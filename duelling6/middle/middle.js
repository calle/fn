var sys = require('sys');
var exec = require('child_process').exec;var http = require('http');



http.createServer(function (req, res) {
//		      var result = execSync("./cmd.sh");

//		      query_backend()
		      exec("./cmd.sh", function(error, stdout, stderr){
			       console.log(stdout);
			   });
//		      console.log(sys);
		      var foo = {question : "haircolor",
				 candidates: [
				     {
    					 name: "Magnus",
					 url: "foo.com/url"
				     },
				     {
					 name: "Moritz",
					 url: "foo.bar/moritz"
				     }
				 ]};
		      
		      res.writeHead(200, {'Content-Type': 'text/plain'});
		      res.end(JSON.stringify(foo));
		  }).listen(8124, "127.0.0.1");
console.log('Server running at http://127.0.0.1:8124/');
