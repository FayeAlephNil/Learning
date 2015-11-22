package collections

class LazyList<E> implements ILazyList<E, LazyList> {
	static def <E> LazyList<E> sequence(Closure<E> closure, LazyList<E> acc = nil()) {
		new LazyList<E>({->
			def head = closure.call(acc)
			new Tuple2(head, sequence(closure, acc.cons(head)).closure)
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
		new LazyList( {-> null} )
	}

	def Closure<Tuple2<E, Closure>> closure

	def LazyList(Closure closure) {
		this.closure = closure
	}

	@Override
	E first() {
		def tuple = closure.call()
		tuple ? tuple.first : null
	}

	@Override
	LazyList<E> head() {
		def tail = tail()
		tail.tail() == nil() ? this : tail.head().cons(first())
	}

	@Override
	LazyList<E> tail() {
		def tuple = closure.call()
		tuple ? new LazyList<E>(tuple.second) : nil()
	}

	@Override
	LazyList<E> cons(E newHead) {
		new LazyList<E>( {-> new Tuple2(newHead, closure)} )
	}

	@Override
	boolean isEmpty() {
		closure.call() == null
	}

	def fold(n, acc, f) {
		n == 0 || isEmpty() ? acc : tail().fold(n-1, f.call(acc, first()), f)
	}

	def foldAll(acc, f) {
		isEmpty() ? acc : tail().foldAll(f.call(acc, first()), f)
	}

	@Override
	<T2> LazyList<T2> map(Closure<T2> f) {
		isEmpty() ? nil() : new LazyList({ -> new Tuple2(f.call(first()), tail().map(f).closure) })
	}

	@Override
	LazyList<E> filter(Closure<Boolean> p) {
		def head = first()
		if (isEmpty()) nil() else {
			(p.call(head)) ? new LazyList({ -> new Tuple2(head, tail().filter(p).closure) }) : tail().filter(p)
		}
	}

	@Override
	def <T2> LazyList<T2> zipWith(ILazyList list, Closure<T2> f) {
		this.isEmpty() ? nil() :
			new LazyList({ ->
				new Tuple2(f.call(first(), list.head()), tail().zipWith(list.tail(), f).closure)
			})
	}
}
