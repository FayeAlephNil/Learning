package striking.learning.math

import scala.collection.{GenTraversableOnce, LinearSeq}

class LogisticMap[A](r: Double = 3.57, start: Double = 0.5, _modifier: LogisticMap.Modifier[A] = LogisticMap.defaultModifier) {
	def modifier = _modifier

	private def lMap(x: Int): Double = x match {
		case 0 => start
		case _ =>
			val last = _get(x - 1)
			r * last * (1 - last)
	}

	private def _get: (Int) => Double = _list.apply

	def get: (Int) => A = list.apply

	private def _list = MathRef.whole.map(lMap)

	def list = modifier(_list)

	def flatMap[B](f: A => GenTraversableOnce[B]): LogisticMap[B] = {
		def nextModifier: LogisticMap.Modifier[B] = { (aList) =>
			modifier(aList).flatMap(f)
		}
		new LogisticMap[B](r, start, nextModifier)
	}

	def map[B](f: A => B): LogisticMap[B] = {
		def nextModifier: LogisticMap.Modifier[B] = { (aList) =>
			modifier(aList).map(f)
		}
		new LogisticMap[B](r, start, nextModifier)
	}

	def filter(f: A => Boolean): LogisticMap[A] = {
		def nextModifier: LogisticMap.Modifier[A] = { (aList) =>
			modifier(aList).filter(f)
		}
		new LogisticMap[A](r, start, nextModifier)
	}
}

object LogisticMap {
	type Modifier[A] = LinearSeq[Double] => LinearSeq[A]

	def defaultModifier: Modifier[Double] = identity
}