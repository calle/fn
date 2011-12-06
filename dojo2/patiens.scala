def sortList(l:List[Int]) : List[Int] = {
	return l
}

def compressList(l:List[Int]) : List[Int] = {
	for(i <- l; if i != 0) yield i;
}

def length(l:List[Int], Int) : List[Int] = {
	
}

def moveCards(l:List[Int]) : List[Int] = {
	var size = length(l, 0);
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

l = compressList(l);
println(l);

//println(moveCards([2,1]) == [2,1])
