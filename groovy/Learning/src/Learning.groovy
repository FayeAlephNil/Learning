import collections.LazyList
import math.Fib

new IntRange(true, -10, 10).each {
	println(Fib.fibNums.get(it))
}

def time(Closure closure) {
	int start = System.currentTimeMillis()
	closure.call()
	start - System.currentTimeMillis()
}

int print = time { println "\n" + 502 }

int sizePrint = time { println "\n" + LazyList.fromStrict(new IntRange(true, 0, 500)).size() }

println sizePrint - print

println LazyList.nil().size()