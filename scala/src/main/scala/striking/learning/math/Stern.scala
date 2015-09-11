package striking.learning.math

import striking.learning.math.MathImps._

import scala.annotation.tailrec
import scala.collection.{GenTraversableOnce, LinearSeq}

class Stern[A](_modifier: Stern.Modifier[A] = Stern.defaultModifier) {
	def modifier = _modifier

	@tailrec
	private def __stern(acc: Int, x: Int): Int = {
		x match {
			case 0 => acc
			case 1 => __stern(acc + 1, 0)
			case _ =>
				if (x.even) {
					__stern(acc, x / 2)
				}
				else {
					val y = x - 1
					val newAcc = acc + _stern(y / 2 + 1)
					__stern(newAcc, y / 2)
				}
		}
	}

	private def _stern: Int => Int = _sterns.apply

	def stern: Int => A = sterns.apply

	private val _sterns: LinearSeq[Int] = MathRef.whole.map(__stern(0, _))

	def sterns: LinearSeq[A] = modifier(_sterns)

	def flatMap[B](f: A => GenTraversableOnce[B]): Stern[B] = {
		def nextModifier: Stern.Modifier[B] = { (aList) =>
			modifier(aList).flatMap(f)
		}
		new Stern[B](nextModifier)
	}

	def map[B](f: A => B): Stern[B] = {
		def nextModifier: Stern.Modifier[B] = { (aList) =>
			modifier(aList).map(f)
		}
		new Stern[B](nextModifier)
	}

	def filter(f: A => Boolean): Stern[A] = {
		def nextModifier: Stern.Modifier[A] = { (aList) =>
			modifier(aList).filter(f)
		}
		new Stern[A](nextModifier)
	}
}

object Stern {
	type Modifier[A] = LinearSeq[Int] => LinearSeq[A]

	val defaultModifier: Modifier[Int] = identity
}
