package main.math

import main.CategoryClasses
import main.collections.LazyList

class Fib {
	public static final fibNums = new Fib(0, 1)
	public static final lucasNums = new Fib(2, 1)

	private final def Number first
	private final def Number second

	private def LazyList makeSeq(Number a, Number b, Closure<Number> closure) {
		new LazyList(a, { makeSeq(b, closure.call(a, b), closure) } )
	}

	public def list = makeSeq(first, second) { a, b -> a + b }
	public def negaList = makeSeq(first, second - first) { a, b -> a - b }

	def Fib(Number first, Number second) {
		this.first = first
		this.second = second
	}

	def get = use(CategoryClasses.ListCategory) { list.andNega(negaList) }
}
