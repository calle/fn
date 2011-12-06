def moveCards(l:List[Int]) : List[Int] = {
	12 :: l
}

def readInput():List[Int] = {

	println("How many?")
	var n = readInt;
	var keepRunning = true;
	/*
	while (keepRunning) {
		try{
			var c = readInt();
			yield c
			// l = c :: l;
			//println(c);
		}
		catch{
			case e: Exception => {keepRunning = false}
		}
	}*/
	
	var i = 0;
	
	for(i=0; i<n; i++){
		
	} yield i;
//	l = while(keepRunning) yield 1;
//	return l
}

var a = 17;
var b = 19;
println(a+b);
var l = readInput();
println(l);
l = moveCards(l);
println(l);