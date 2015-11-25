package main

import main.collections.LazyList
import main.math.Fib

def time(Closure closure) {
	int start = System.currentTimeMillis()
	closure.call()
	System.currentTimeMillis() - start
}


use(CategoryClasses.IntegerCategory) {
	new IntRange(true, -10, 10).each {
		println Fib.fibNums.pisano(7).get(it) + ", " + Fib.lucasNums.pisano(7).get(it)
		println "$it.fib, $it.lucas \n"
	}

	int print = time { println "\n" + 502 }

	int sizePrint = time { println "\n" + LazyList.fromStrict(new IntRange(true, 0, 500)).size() }

	println sizePrint - print

	println LazyList.nil().size() + "\n"
	println time { println 1000.factorial } + "\n"

	println LazyList.fromStrict([1,2,3,4,5]).equals(LazyList.fromStrict([1,2,3,4,5]))
	println LazyList.fromStrict([1,2,3,4, 5]).equals(LazyList.fromStrict([1,2,3,4]))
}
