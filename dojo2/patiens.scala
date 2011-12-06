def moveCards(l:List[Int]) : List[Int] = {
	12 :: l
}

def readInput():List[Int] = {

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
	
	l = while(keepRunning) yield 1;
	return l
}

var a = 17;
var b = 19;
println(a+b);
var l = readInput();
println(l);
l = moveCards(l);
println(l);