package test

import main.collections.LazyList
import main.math.MathRefs
import spock.lang.*

class LazyListSpec extends Specification {
	def "Creating a list using cons and obtaining elements"() {
		expect:
		def list = LazyList.nil().cons(c).cons(b).cons(a).head()
		list.first() == a
		list.tail().first() == b
		list.get(2) == c

		where:
		a << [1, "String", LazyList.nil().cons(1)]
		b << [4, "Meow", LazyList.nil().cons(4)]
		c << [2, "Life", LazyList.nil().cons(2)]
	}

	def "Creating a list using fromStrict and forcing it"() {
		expect:
		def list = LazyList.fromStrict(strict)
		list.first() == strict.first()
		list.get(1) == strict.get(1)
		list.get(2) == strict.get(2)

		where:
		strict << [[1,2,3], [2,3,1], ["Meow", "Life", "Shhh"], [[1,2], [54, 32], [4,32,1]]]
	}

	def "Checking if a list is empty works correctly"() {
		expect:
		def list = LazyList.fromStrict(strict)
		list.empty == strict.empty

		where:
		strict << [[], [1], [2,3], [[]]]
	}

	def "Sequencing works fine"() {
		expect:
		MathRefs.whole.get(idx) == idx
		MathRefs.natural.get(idx) == idx + 1

		where:
		idx << new IntRange(true, 0, 100)
	}

	def "Take works, even on infinite lists"() {
		expect:
		list.take(0) == expected.take(0)
		list.take(toTake) == expected.take(toTake)
		list.take(toTake + 2) == expected.take(toTake + 2)
		list.take(toTake * 2) == expected.take(toTake * 2)

		where:
		expected << [new IntRange(true, 1, 100), [1,2,3,4,5]]
		list << [MathRefs.natural, LazyList.fromStrict([1, 2, 3, 4, 5])]
		toTake << [1,3]
	}
}
