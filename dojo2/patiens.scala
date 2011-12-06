def readInput():List[Int] = {
	var l:List[Int] = Nil;
	while (true){
		try{
			var c = readInt;
			c :: l;
		}
		catch{
			case e: Exception =>
				return l;
		}
	}
}

var a = 17;
var b = 19;
println(a+b);
var l = readInput();
println(l);