package main.math

import main.CategoryClasses
import main.collections.LazyList

class Fib {
	public static final fibNums = new Fib(0, 1)
	public static final lucasNums = new Fib(2, 1)

	private def LazyList makeSeq(Number a, Number b, Closure<Number> closure) {
		new LazyList(a, { makeSeq(b, closure.call(a, b), closure) } )
	}

	public final def LazyList list
	public final def LazyList negaList

	def Fib(Number first, Number second) {
		this.list = makeSeq(first, second) { a, b -> a + b }
		this.negaList = makeSeq(first, second - first) { a, b -> a - b }
	}

	private def Fib(LazyList list, LazyList negaList) {
		this.list = list
		this.negaList = negaList
	}

	def get(int idx) {
		use(CategoryClasses.ListCategory) { list.andNega(negaList)(idx) }
	}

	def Fib pisano(int n) {
		this.map {
			it % n
		}
	}

	def methodMissing(String name, args) {
		try {
			return new Fib(list."$name"(*args) as LazyList, negaList."$name"(*args) as LazyList)
		} catch (NoSuchMethodError e) {
			throw new NoSuchMethodError(e.localizedMessage)
		} catch (NoSuchMethodException e) {
			throw new NoSuchMethodException(e.localizedMessage)
		} catch (ClassCastException e) {
			throw new NoSuchMethodException("No such method on Fib: $name")
		} catch (MissingMethodException e) {
			throw new MissingMethodException(name, this.class, args, false)
		}
	}
}
