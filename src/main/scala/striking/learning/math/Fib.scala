package striking.learning.math

import scala.collection.LinearSeq

class Fib(n: BigInt, m: BigInt) {
	def get(x:Int): BigInt = {
		def new_fibs(x:Int):(BigInt,BigInt) = if (x == 0) (n, m) else {
			val (a,b) = new_fibs(x/2)
			val p = (2*b+a)*a
			val q = a*a + b*b
			if(x % 2 == 0) (p,q) else (p+q,p)
		}
		if (x < 0) {
			val n = -x
			BigInt(-1).pow(n+1) * new_fibs(n)._1
		} else {
			new_fibs(x)._1
		}
	}

	def list: LinearSeq[BigInt] = Reference.whole.map(get)
	def negaList: LinearSeq[BigInt] = Reference.negative.map(get)
}