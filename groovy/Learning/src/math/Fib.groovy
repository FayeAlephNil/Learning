package math

import collections.CollectionUtil
import collections.LazyList

class Fib {
	public static final fibNums = new Fib(0, 1)

	private final def Number first
	private final def Number second

	public def list = LazyList.sequence { LazyList<Number> before ->
		def tail = before.tail()
		if (before.empty) {
			first
		} else if (tail.empty) {
			second
		} else {
			before.first() + before.get(1)
		}
	}

	public def negaList = LazyList.sequence { LazyList<Number> before ->
		def tail = before.tail()
		if (before.empty) {
			first
		} else if (tail.empty) {
			second - first
		} else {
			before.get(1) - before.first()
		}
	}

	def Fib(Number first, Number second) {
		this.first = first
		this.second = second
	}

	def get = CollectionUtil.listAndNega(list, negaList)
}
