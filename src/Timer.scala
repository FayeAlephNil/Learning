class Timer(callback: () => Unit, time: Int) {

	new Thread(new Runnable {
		override def run(): Unit = {
			while (running) {
				callback()
				this.wait(time)
			}
		}
	})

	var running = false

	def start: Unit = {
		running = true
	}

	def stop: Unit = {
		running = false
	}
}
