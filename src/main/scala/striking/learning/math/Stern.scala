package striking.learning.math

import MathImps._

import scala.annotation.tailrec
import scala.collection.{GenTraversableOnce, LinearSeq}

class Stern(_modifier: Stern.Modifier = Stern.defaultModifier) {
	def modifier = _modifier

	@tailrec
	private def _stern(acc: Int, x: Int): Int = {
		x match {
			case 0 => acc
			case 1 => _stern(acc + 1, 0)
			case _ =>
				if (x.even) {
					_stern(acc, x / 2)
				}
				else {
					val y = x - 1
					val newAcc = acc + stern(y / 2 + 1)
					_stern(newAcc, y / 2)
				}
		}
	}

	def stern(x: Int): Int = {
		sterns.apply(x)
	}

	val _sterns: LinearSeq[Int] = MathRef.whole.map(_stern(0, _))

	def sterns: LinearSeq[Int] = modifier(_sterns)

	def flatMap(f: Int => GenTraversableOnce[Int]): Stern = {
		def nextModifier: Stern.Modifier = { (aList) =>
			modifier(aList).flatMap(f)
		}
		new Stern(nextModifier)
	}

	def map(f: Int => Int): Stern = {
		def nextModifier: Stern.Modifier = { (aList) =>
			modifier(aList).map(f)
		}
		new Stern(nextModifier)
	}

	def filter(f: Int => Boolean): Stern = {
		def nextModifier: Stern.Modifier = { (aList) =>
			modifier(aList).filter(f)
		}
		new Stern(nextModifier)
	}
}

object Stern {
	type Modifier = LinearSeq[Int] => LinearSeq[Int]

	val defaultModifier: Modifier = identity
}
