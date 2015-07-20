object Learning extends App {
	println("Hello World!")
	new Timer(() => println("1000 milliseconds have passed"), 1000).start
	new Timer(() => println("2000 milliseconds have passed"), 2000).start
}