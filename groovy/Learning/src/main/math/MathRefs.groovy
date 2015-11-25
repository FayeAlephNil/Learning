package main.math

import main.CategoryClasses
import main.collections.LazyList

class MathRefs {
	static def natural = LazyList.sequence { LazyList<Integer> before ->
		before.empty() ? 1 : before.first() + 1
	}

	static def whole = natural.cons(0)

	static def negative = LazyList.sequence { LazyList<Integer> before ->
		before.empty ? -1 : before.first() - 1
	}

	static def integers = use (CategoryClasses.ListCategory) { whole.andNega(negative) }
}
