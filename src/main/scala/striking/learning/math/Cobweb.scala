package striking.learning.math

import striking.learning.math.MathImps._

import scala.collection.LinearSeq

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
		mapTuple(Cobweb.tupleFunc(g))
	}

	def filterTuple(g: ((B, B)) => Boolean): Cobweb[A, B] = {
		def nextModifier: Cobweb.Modifier[A, B] = { (aList) =>
			modifier(aList).filter(g)
		}
		new Cobweb[A, B](f, start, nextModifier)
	}

	def filter(g: B => Boolean): Cobweb[A, B] = {
		val and: ((Boolean, Boolean)) => Boolean = { (t: (Boolean, Boolean)) =>
			t._1 && t._2
		}
		filterTuple(Cobweb.tupleFuncMod(g, and))
	}
}

object Cobweb {
	type Modifier[A, B] = LinearSeq[(A, A)] => LinearSeq[(B, B)]

	def defaultModifier[A]: Modifier[A, A] = identity

	protected def tupleFuncMod[A, B, C](f: A => B, g: ((B, B)) => C): ((A, A)) => C = {
		t: (A, A) => g(f(t._1), f(t._2))
	}

	protected def tupleFunc[A, B](f: A => B): ((A, A)) => (B, B) = {
		val func: ((B, B)) => (B, B) = (t: (B, B)) => t
		tupleFuncMod(f, func)
	}
}