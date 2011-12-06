def moveCards(l:List[Int]) : List[Int] = {
	return l
}

def readInput():List[Int] = {
	var l:List[Int] = Nil;
	var keepRunning = true;
	while (keepRunning) {
		try{
			var c = readInt();
			l = c :: l;
		}catch{
			case e: Exception => {keepRunning = false}
		}
	}
	l;
}

var l = readInput();

l = moveCards(l);

println(l);

println(moveCards([2,1]) == [2,1])
