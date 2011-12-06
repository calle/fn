def compressList(l:List[Int]) : List[Int] = {
	for(i <- l; if i > 0) yield i;
}

def moveCards(l:List[Int]) : List[Int] = {
	val j = for(i <- l) yield i-1
	(compressList(l.size :: j)).sortWith(_>_)
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

def bulgaric_patiens(){
	//var l = readInput();
	
	//while(changed){
//		moveCards
//	}
}

var l = List(1,2,3);
println(l);
for (i <- Range(0,25)) {
	moveCards(l);
}
println(l);

println(moveCards(List(0, -1, 1, 2,3)))
println(1, moveCards(List(2,1)) == List(2,1))
println(2, moveCards(List(1,2,3)) == List(3,2,1))
println(3, moveCards(List(0,-1,1,2,3)) == List(5,2,1))


