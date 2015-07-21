package striking.learning.math

import striking.learning.Implicits

import scala.collection.LinearSeq
import Implicits._

class Fib(n: BigInt, m: BigInt) {
	private def new_fibs(x:Int):(BigInt,BigInt) = if (x == 0) (n, m) else if (x < 0) {
		val n = -x
		val result = BigInt(-1).pow(n + 1) * get(n)
		(result, 0)
	} else {
		val (a,b) = getTuple(x/2)
		val p = (2*b+a)*a
		val q = a*a + b*b
		if(x % 2 == 0) (p,q) else (p+q,p)
	}

	private def getTuple: (Int => (BigInt, BigInt)) = listTuple.andNega(negaListTuple)

	def get: (Int => BigInt) = list.andNega(negaList)

	private val listTuple: LinearSeq[(BigInt, BigInt)] = Reference.whole.map(new_fibs)
	private val negaListTuple: LinearSeq[(BigInt, BigInt)] = Reference.negative.map(new_fibs)

	def list: LinearSeq[BigInt] = listTuple.map(tuple => tuple._1)
	def negaList: LinearSeq[BigInt] = negaListTuple.map(tuple => tuple._1)

	def regenWithGen(regen: Fib => (BigInt, BigInt)): Fib = {
		val (a, b) = regen(this)
		new Fib(a, b)
	}

	def regenWithIndex(a: Int, b: Int, regen: (BigInt => BigInt)): Fib = {
		regenWithGen(gen => {
			(regen(gen.get(a)), regen(gen.get(b)))
		})
	}

	def regenWith: (BigInt => BigInt) => Fib = regenWithIndex(0, 1, _)
}