package collections

trait ILazyList<E, T extends ILazyList> {
	abstract static def T nil()

	abstract def E first()

	abstract def T head()

	abstract def T tail()

	abstract def T cons(E newHead)

	abstract def boolean isEmpty()

	abstract def fold(n, acc, f)

	abstract def foldAll(acc, f)

	abstract def <T2> T map(Closure<T2> f)

	abstract def T filter(Closure<Boolean> p)

	abstract def <T2> T zipWith(ILazyList list, Closure<T2> f)

    static def Iterator<E> iterator(ILazyList<E, ILazyList> list) {
		new AnIterator<E>(list)
	}

	static def E lastRec(ILazyList<E, ? extends ILazyList> list) {
		def tail = list.tail()
		tail.isEmpty() ? list.first() : lastRec(tail)
	}

	def last() {
		lastRec(this)
	}

	def empty() {
		isEmpty()
	}

	def iterator() {
		iterator(this)
	}

	def List<E> toList() {
		takeAll()
	}

	def int size() {
		def count = 0
		this.each { count++ }
		count
	}

	def E get(int idx) {
		if (idx == 0) first() else tail().get(idx - 1)
	}

	def List<E> take(int amount) {
		fold(n, []) { acc, item -> acc << item} as List<E>
	}

	def List<E> takeAll() {
		foldAll([]) { acc, item -> acc << item } as List<E>
	}

	def boolean equals(Object o) {
		if (this == null && o.equals(nil())) return true
		def isList = o instanceof ILazyList<E, ? extends ILazyList>
		if (isList) {
			def list = o as ILazyList<E, ? extends ILazyList>
			def headsEqual = list.first().equals(this.first())
			headsEqual && ((list.tail() == list.nil() && this.tail() == nil()) || list.tail().equals(this.tail()))
		} else {
			false
		}
	}

	private static class AnIterator<E> implements Iterator<E> {
		def ILazyList<E, ILazyList> list

		def AnIterator(ILazyList<E, ILazyList> list) {
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