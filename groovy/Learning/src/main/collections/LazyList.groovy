package main.collections

import groovy.transform.CompileStatic
import groovy.transform.TailRecursive

class LazyList<E> {
	static def <E> LazyList<E> sequence(Closure<E> closure, LazyList<E> acc = nil()) {
		def E first = closure.call(acc)

		new LazyList(first, {->
			sequence(closure, acc.cons(first))
		})
	}

	static def fromStrict(List<E> strict) {
		def list = nil()
		strict.reverseEach {
			list = list.cons(it)
		}
		list
	}

	static def <E> LazyList<E> nil() {
		new LazyList({ 0 }, {-> null})
	}

	private def Closure<Tuple2<E, Closure>> closure
	private def Closure<Integer> size

	private def LazyList(Closure<Integer> size, Closure closure) {
		this.closure = closure
		this.size = size
	}

	private static def sizeOfClosure(Closure<Tuple2<E, Closure>> closure, int acc = 0) {
		return {
			def tuple = closure.call()
			tuple == null ? acc : sizeOfClosure(tuple.second, acc + 1).call()
		}
	}

	private static def lazyCons(E first, Closure<Tuple2<E, Closure>> closure) {
		new LazyList({1 + sizeOfClosure(closure).call()}, { new Tuple2(first, closure) })
	}

	def LazyList(E first, Closure<LazyList<E>> closure) {
		def list = lazyCons(first, {
			def list = closure.call()
			list == null ? null : new Tuple2(list.first(), list.tail().closure)
		})
		this.size = list.size
		this.closure = list.closure
	}

	E first() {
		def tuple = closure.call()
		tuple ? tuple.first : null
	}

	LazyList<E> head() {
		def tail = tail()
		tail.tail() == nil() ? this : tail.head().cons(first())
	}

	LazyList<E> tail() {
		def tuple = closure.call()
		tuple ? new LazyList<E>({ size() - 1}, tuple.second) : nil()
	}

	LazyList<E> cons(E newHead) {
		new LazyList<E>({1 + size.call()}, {-> new Tuple2(newHead, closure)} )
	}

	boolean isEmpty() {
		closure.call() == null
	}

	def fold(n, acc, f) {
		n == 0 || isEmpty() ? acc : tail().fold(n-1, f.call(acc, first()), f)
	}

	def foldAll(acc, f) {
		isEmpty() ? acc : tail().foldAll(f.call(acc, first()), f)
	}

	def <T2> LazyList<T2> map(Closure<T2> f) {
		isEmpty() ? nil() : new LazyList(size, { -> new Tuple2(f.call(first()), tail().map(f).closure) })
	}

	LazyList<E> filter(Closure<Boolean> p) {
		def first = first()
		if (isEmpty()) nil() else {
			def tail = tail().filter(p)
			(p.call(first)) ? new LazyList({1 + tail.size() },  { -> new Tuple2(first, tail.closure) }) : tail
		}
	}

	def <T2> LazyList<T2> zipWith(LazyList list, Closure<T2> f) {
		this.isEmpty() ? nil() :
			new LazyList(this.size, { ->
				new Tuple2(f.call(first(), list.first()), tail().zipWith(list.tail(), f).closure)
			})
	}

	def last(LazyList<E> list = this) {
		def tail = list.tail()
		tail.isEmpty() ? list.first() : last(tail)
	}

	def empty() {
		isEmpty()
	}

	def Iterator<E> iterator() {
		new AnIterator<E>(this)
	}

	def List<E> toList() {
		takeAll()
	}

	def int size() {
		size.call()
	}

	def E get(int idx) {
		if (idx == 0) first() else tail().get(idx - 1)
	}

	def List<E> take(int amount) {
		fold(amount, []) { acc, item -> acc << item} as List<E>
	}

	def List<E> takeAll() {
		foldAll([]) { acc, item -> acc << item } as List<E>
	}

	def boolean equals(Object o) {
		equalsStatic(o, this)
	}

	@CompileStatic
	@TailRecursive
	private static def boolean equalsStatic(Object o, LazyList<E> self, boolean acc = true) {
		if (self == null && equalsStatic(o, nil())) return true
		def isList = o instanceof LazyList<E>
		if (isList) {
			def list = o as LazyList<E>
			if (list.empty && self.empty) {
				acc
			} else if (list.empty || self.empty) {
				false
			} else {
				def firstsEqual = list.first().equals(self.first())
				equalsStatic(list.tail(), self.tail(), firstsEqual)
			}
		} else {
			false
		}
	}

	private static class AnIterator<E> implements Iterator<E> {
		def LazyList<E> list

		def AnIterator(LazyList<E> list) {
			this.list = list
		}

		@Override
		boolean hasNext() {
			!list.isEmpty()
		}

		@Override
		E next() {
			def result = list.first()
			list = list.tail()
			result
		}
	}
}
