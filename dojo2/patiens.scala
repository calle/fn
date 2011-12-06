def compressList(l:List[Int]) : List[Int] = {
	for(i <- l; if i > 0) yield i;
}

def moveCards(l:List[Int]) : List[Int] = {
	val j = for(i <- l) yield i-1
	(compressList(l.size :: j)).sortWith(_>_)
}

def bulgaricPatiens(l: List[Int]) : Boolean = {	
	for (i <- Range(0,25)) {
		if(l == moveCards(l)){
			return true
		}
	}
	return false
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

println(1, moveCards(List(2,1)) == List(2,1))
println(2, moveCards(List(1,2,3)) == List(3,2,1))
println(3, moveCards(List(0,-1,1,2,3)) == List(5,2,1))
println("Should be true", bulgaricPatiens(List(3,2,1)))
println("Should be false", bulgaricPatiens(List(3,2)))

