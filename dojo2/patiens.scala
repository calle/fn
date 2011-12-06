def readInput(){
	while (true){
		try{
			var c = readInt;
			println(c)
		}
		catch{
			case e: Exception =>
				print("exception");
				return;
		}
	}
}

var a = 17;
var b = 19;
println(a+b);
readInput()
