package org.kframework.kale.util

object timer {

  class ReentrantTimer(name: String) {
    private var _entries = 0
    private var _totalTime = 0L
    private var _lastEntry = 0L
    private var _hits = 0L
    private var _errorHits = 0L

    @inline private final def enter(): Unit = {
      if (_entries == 0) {
        _lastEntry = System.nanoTime()
      }
      _entries += 1
      _hits += 1
    }

    @inline private final def exit(): Unit = {
      _entries -= 1
      if (_entries == 0) {
        _totalTime += (System.nanoTime() - _lastEntry)
      }
    }

    @inline final def apply[T](f: => T): T = {
      enter()
      val res = try {
        f
      } catch {
        case e: Throwable => _errorHits += 1; throw e;
      } finally {
        exit()
      }
      res
    }

    def totalTime: Long = _totalTime

    def hits: Long = _hits

    def errorHits: Long = _errorHits

    def report: String = {
      name + ": time = " + formatTime(totalTime) + ";  hits: " + _hits +
        (if (_errorHits > 0) "errorHits: " + _errorHits else "")
    }
  }

  private val rs = collection.mutable.Map[String, ReentrantTimer]()

  @inline def apply[T](name: String): ReentrantTimer = rs.getOrElseUpdate(name, new ReentrantTimer(name))

  @inline def reset(name: String) = {
    rs.remove(name)
  }

  def formatTime(l: Long): String = {
    import scala.concurrent.duration._
    l.nanos.toMillis + "ms"
  }

  def fullReport(): String = {
    rs.values map (_.report) mkString "\n\n"
  }

  def fullReset(): Unit = {
    rs.clear()
  }
}
