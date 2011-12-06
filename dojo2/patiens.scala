def sortList(l:List[Int]) : List[Int] = {
	return l
}

def compressList(l:List[Int]) : List[Int] = {
	for(i <- l; if i != 0) yield i;
}

def moveCards(k:List[Int]) : List[Int] = {
	val l = compressList(k)
	val j = for(i <- l) yield i-1
	l.size :: j
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



l = compressList(l);
println(l);

l = moveCards(l);
println(l);

assert(moveCards(List(2,1)) == List(2,1))
