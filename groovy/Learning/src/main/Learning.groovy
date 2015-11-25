package main

import main.collections.LazyList

def time(Closure closure) {
	int start = System.currentTimeMillis()
	closure.call()
	start - System.currentTimeMillis()
}


use(CategoryClasses.IntegerCategory) {
	new IntRange(true, -10, 10).each {
		println "$it.fib, $it.lucas"
	}

	int print = time { println "\n" + 502 }

	int sizePrint = time { println "\n" + LazyList.fromStrict(new IntRange(true, 0, 500)).size() }

	println sizePrint - print

	println LazyList.nil().size() + "\n"
	println 20.factorial
}
