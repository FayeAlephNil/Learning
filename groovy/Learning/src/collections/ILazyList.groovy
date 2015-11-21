package collections

import collections.ILazyList.AnIterator
import groovy.transform.TailRecursive

trait ILazyList<E, T extends ILazyList<E, T>> {
	abstract static def T nil()

	abstract def E head()

	abstract def T tail()

	abstract def T cons(E newHead)

    static def Iterator<E> iterator(ILazyList<E, ILazyList> list) {
		new AnIterator<E>(list)
	}

	def iterator() {
		iterator(this)
	}

	@TailRecursive
	def List<E> recForce(ArrayList<E> acc) {
		if (empty) acc else recForce(acc.add(0, head()))
	}

	def List<E> force() {
		recForce(new ArrayList<E>())
	}

	def int size() {
		def count = 0
		this.each { count++ }
		count
	}

	def E get(int idx) {
		if (idx == 0) head() else tail().get(idx - 1)
	}

	def T take(int amount) {
		def count = amount
		def list = nil()
		this.each {
			if (count != 0) {
				list = list.add(it)
			}
			count--
		}
		list
	}

	def boolean equals(Object o) {
		if (this == null && o.equals(nil())) return true
		def isList = o instanceof ILazyList<E, ? extends ILazyList>
		if (isList) {
			def list = o as ILazyList<E, ? extends ILazyList>
			def headsEqual = list.head().equals(this.head())
			headsEqual && ((list.tail() == list.nil() && this.tail() == nil()) || list.tail().equals(this.tail()))
		} else {
			false
		}
	}

	private static class AnIterator<E> implements Iterator<E> {
		def list

		def AnIterator(ILazyList<E, ILazyList> list) {
			this.list = list
		}

		@Override
		boolean hasNext() {
			!list.head().equals(null) || !list.tail().equals(nil())
		}

		@Override
		E next() {
			def result = list.head()
			list = list.tail()
			result
		}
	}
}