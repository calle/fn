def moveCards(l:List[Int]) : List[Int] = {
	var list = l :: 12;
	return list;
}

def readInput():List[Int] = {
	var l:List[Int] = Nil;
	while (true) {
		try{
			var c = readInt;
			l = c :: l;
			//println(c);
		}
		catch{
			case e: Exception =>
				return l;
		}
	}
	return Nil;
}

var a = 17;
var b = 19;
println(a+b);
var l = readInput();
println(l);
l = moveCards(l);
println(l);