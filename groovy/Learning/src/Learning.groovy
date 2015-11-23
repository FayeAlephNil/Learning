import collections.LazyList
import math.Fib

new IntRange(true, -10, 10).each {
	println(Fib.fibNums.get(it))
}

println "\n" + LazyList.fromStrict(new IntRange(true, 0, 100)).size()