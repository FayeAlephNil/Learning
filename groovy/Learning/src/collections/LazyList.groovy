package collections

class LazyList<E> implements ILazyList<E, LazyList<E>> {
	private def head = null
	private def tail

	static def LazyList<E> nil() {
		new LazyList<E>(null, null as LazyList<E>)
	}

	static def LazyList<E> fromIter(Iterator<E> iter) {
		if (iter.hasNext()) {
			new LazyList<E>(iter.next(), iter)
		} else {
			nil()
		}
	}

	static def <S> LazyList<E> fromGen(Closure<Tuple2<E, S>> closure, S beginState) {
		fromIter(new StateIterator(closure, beginState))
	}

	private def LazyList(List<E> elems) {
		def list = nil()

		elems.each {
			list = list.cons(it)
		}
		list.reverse()
	}

	private def LazyList(E head, LazyList<E> tail) {
		this.head = head
		if (tail == null) this.tail = nil() else this.tail = tail
	}

	private def LazyList(E head, Iterator<E> tail) {
		this.head = head
		this.tail = tail
	}

	LazyList<E> reverse() {
		def list = nil()
		this.each {
			list = list.cons(it as E)
		}
		list
	}

	@Override
	E head() {
		head
	}

	@Override
	LazyList<E> tail() {
		if (tail instanceof Iterator<E>) fromIter(tail) else tail
	}

	@Override
	LazyList<E> cons(E head) {
		new LazyList<E>(head, this)
	}

	private static class StateIterator<E, S> implements Iterator<E> {
		def S state
		def closure

		def StateIterator(Closure<Tuple2<E, S>> closure, S beginState) {
			state = beginState
			this.closure = closure
		}

		@Override
		boolean hasNext() {
			return true
		}

		@Override
		E next() {
			Tuple2<E, S> tuple = closure.call(state)
			state = tuple.second
			tuple.first
		}
	}
}
