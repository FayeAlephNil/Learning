package striking.learning.math

import striking.learning.Implicits._

import scala.collection.LinearSeq

class Fib(n: BigInt, m: BigInt, modifier: Fib.Modifier = Fib.defaultModifier) {
	private val self = this

	def get: (Int => BigInt) = list.andNega(negaList)

	private val _list: LinearSeq[BigInt] = MathRef.numStreamBig(n, m, (x, y) => x + y)
	private val _negaList: LinearSeq[BigInt] = MathRef.numStreamBig(n, m - n, (x, y) => y - x)

	val (list, negaList) = modifier(_list, _negaList)

	def regenWithGen(regen: Fib => (BigInt, BigInt)): Fib = {
		val (a, b) = regen(this)
		new Fib(a, b, modifier)
	}

	def regenWithIndex(a: Int, b: Int, regen: (BigInt => BigInt)): Fib = {
		regenWithGen { gen =>
			(regen(gen.get(a)), regen(gen.get(b)))
		}
	}

	def regenWith: (BigInt => BigInt) => Fib = regenWithIndex(0, 1, _)

	def map(f: BigInt => BigInt): Fib = {
		def nextModifier: Fib.Modifier = { (aList, aNegaList) =>
			modifier(aList, aNegaList).map(_.map(f))
		}
		new Fib(n, m, nextModifier)
	}

	def filter(f: BigInt => Boolean): Fib = {
		def nextModifier: Fib.Modifier = { (aList, aNegaList) =>
			modifier(aList, aNegaList).map(_.filter(f))
		}
		new Fib(n, m, nextModifier)
	}

	def pisano(y: Int): Fib = {
		def modY(x: BigInt): BigInt = x % y
		map(modY)
	}
}

object Fib {
	type Modifier = (LinearSeq[BigInt], LinearSeq[BigInt]) => (LinearSeq[BigInt], LinearSeq[BigInt])

	def defaultModifier: Modifier = { (list, negaList) =>
		(list, negaList)
	}
}