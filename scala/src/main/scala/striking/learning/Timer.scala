package striking.learning

import scala.collection.mutable.ArrayBuffer

object Timers {
	var timers = new ArrayBuffer[Timer]()

	def addTimer(timer: Timer): Unit = {
		timers += timer
	}

	new Thread(new Runnable {
		override def run(): Unit = {
			var passed = 0
			while (true) {
				timers.foreach(timer => {
					if (timer.started && (passed % timer.time == 0) && (passed != 0)) {
						timer.run
					}
				})

				Thread.sleep(1)
				passed += 1
			}
		}
	}).start()
}

class Timer(callback: Timer => Unit, _time: Int, add: Boolean = true) {
	if (add) Timers.addTimer(this)


	private var _started = false
	def started = _started

	def time = _time

	def run: Unit = {
		callback(this)
	}

	def start: Timer = {
		_started = true
		this
	}

	def stop: Timer = {
		_started = false
		this
	}
}

