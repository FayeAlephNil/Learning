package striking.learning.math

import striking.learning.Implicits._

import scala.collection.LinearSeq

class Fib[A](n: BigInt, m: BigInt, _modifier: Fib.Modifier[A] = Fib.defaultModifier) {
	def get: (Int => A) = list.andNega(negaList)

	def modifier = _modifier

	private val _list: LinearSeq[BigInt] = MathRef.numStreamBig(n, m, (x, y) => x + y)
	private val _negaList: LinearSeq[BigInt] = MathRef.numStreamBig(n, m - n, (x, y) => y - x)

	val (list, negaList) = modifier(_list, _negaList)

	def map[B](f: A => B): Fib[B] = {
		def nextModifier: Fib.Modifier[B] = { (aList, aNegaList) =>
			modifier(aList, aNegaList).map(_.map(f))
		}
		new Fib(n, m, nextModifier)
	}

	def filter(f: A => Boolean): Fib[A] = {
		def nextModifier: Fib.Modifier[A] = { (aList, aNegaList) =>
			modifier(aList, aNegaList).map(_.filter(f))
		}
		new Fib(n, m, nextModifier)
	}
}

object Fib {
	type Modifier[A] = (LinearSeq[BigInt], LinearSeq[BigInt]) => (LinearSeq[A], LinearSeq[A])

	def defaultModifier: Modifier[BigInt] = { (list, negaList) =>
		(list, negaList)
	}
}