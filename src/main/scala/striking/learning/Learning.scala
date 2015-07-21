package striking.learning
object Learning extends App {
	println("Hello World!")
	new Timer(timer => println("1000 milliseconds have passed"), 1000).start
	new Timer(timer => println("2000 milliseconds have passed"), 2000).start
	new Timer(timer => {
		println("3000 milliseconds have passed, Adios")
		timer.stop
	}, 3000).start
}
