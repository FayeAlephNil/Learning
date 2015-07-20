class Timer(callback: () => Unit, time: Int) {

	new Thread(new Runnable {
		override def run(): Unit = {
			while (true) {
				if (running) {
					Thread sleep time
					callback()
				}
			}
		}
	}).start()

	var running = false

	def start: Unit = {
		running = true
	}

	def stop: Unit = {
		running = false
	}
}
