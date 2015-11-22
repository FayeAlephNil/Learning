package math

import collections.CollectionUtil
import collections.LazyList

class Fib {
	public static final fibNums = new Fib(0, 1)

	private final def Number first
	private final def Number second

	private def LazyList makeSeq(Number a, Number b, Closure<Number> closure) {
		new LazyList( {-> new Tuple2(a, makeSeq(b, closure.call(a, b), closure).closure)} )
	}

	public def list = makeSeq(first, second) { a, b -> a + b }
	public def negaList = makeSeq(first, second - first) { a, b -> a - b }

	def Fib(Number first, Number second) {
		this.first = first
		this.second = second
	}

	def get = CollectionUtil.listAndNega(list, negaList)
}
