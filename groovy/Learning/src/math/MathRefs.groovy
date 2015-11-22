package math

import collections.LazyList

class MathRefs {
	static def natural = LazyList.sequence { LazyList<Integer> before ->
		before.empty() ? 1 : before.head() + 1
	}
}
