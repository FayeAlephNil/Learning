package math

import collections.CollectionUtil
import collections.LazyList

class Fib {
	public static final fibNums = new Fib(0, 1)

	private final def Integer first
	private final def Integer second

	public def list = LazyList.fromGen({ def state ->
		def result = state.first + state.second
		new Tuple2<>(result, new Tuple2(state.second, result))
	}, new Tuple2<>(first, second))

	public def negaList = LazyList.fromGen({ def state ->
		def result = state.second - state.first
		new Tuple2<>(result, new Tuple2(state.second, result))
	}, new Tuple2<>(first, second - first))

	def Fib(Number first, Number second) {
		this.first = first
		this.second = second
	}

	def get = CollectionUtil.listAndNega(list, negaList)
}
