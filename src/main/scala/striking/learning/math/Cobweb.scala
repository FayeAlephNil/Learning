package striking.learning.math

import scala.collection.{GenTraversableOnce, LinearSeq}
import MathImps._

class Cobweb[A, B](f: A => A, start: A, _modifier: Cobweb.Modifier[A, B] = Cobweb.defaultModifier[A]) {
	def modifier = _modifier

	private def _get(x: Int): (A, A) = {
		val a = if (x != 0) _get(x - 1) else (start, start)
		if (x.even) {
			(a._1, f(a._2))
		} else {
			(f(a._1), a._2)
		}
	}

	def get: Int => (B, B) = list.apply

	private def _list = MathRef.whole.map(_get)

	def list = modifier(_list)

	def mapTuple[C](g: ((B, B)) => (C, C)): Cobweb[A, C] = {
		def nextModifier: Cobweb.Modifier[A, C] = { (aList) =>
			modifier(aList).map[(C, C), LinearSeq[(C, C)]](g)
		}
		new Cobweb[A, C](f, start, nextModifier)
	}

	def map[C](g: B => C): Cobweb[A, C] = {
		mapTuple(t => (g(t._1), g(t._2)))
	}

	def filter(g: ((B, B)) => Boolean): Cobweb[A, B] = {
		def nextModifier: Cobweb.Modifier[A, B] = { (aList) =>
			modifier(aList).filter(g)
		}
		new Cobweb[A, B](f, start, nextModifier)
	}

}

object Cobweb {
	type Modifier[A, B] = LinearSeq[(A, A)] => LinearSeq[(B, B)]

	def defaultModifier[A]: Modifier[A, A] = identity
}