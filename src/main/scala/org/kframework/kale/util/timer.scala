package org.kframework.kale.util

import io.circe.Json

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object timer {

  private val rs = collection.mutable.Map[String, Timer]()

  def register[T <: Timer](name: String, t: => T): T = {
    rs.getOrElseUpdate(t.name, t).asInstanceOf[T]
  }

  def apply(name: String): Timer = register(name, new Timer(name))

  def timers: Map[String, Timer] = rs.toMap

  def get(name: String): Option[Timer] = rs.get(name)

  def formatTime(l: Long): String = {
    l.nanos.toMillis + "ms"
  }

  def fullReport(): String = {
    rs.values map (_.report) mkString "\n\n"
  }

  def fullReset(): Unit = {
    rs.values.foreach(_.reset())
  }

  class Timer(val name: String) {
    protected[this] var _entries = 0
    protected[this] var _totalTime = 0L
    protected[this] var _lastEntry = 0L
    protected[this] var _hits = 0L
    protected[this] var _errorHits = 0L
    protected[this] var _invocations = 0L

    def reset() = {
      //      assert(isInside, "Do not reset the timer during measuring.")
      _entries = 0
      _totalTime = 0L
      _lastEntry = 0L
      _hits = 0L
      _errorHits = 0L
      _invocations = 0L
    }

    def isOutside = _entries == 0

    def atMost[T](f: => T, time: Duration): T = {
      val future = Future[T] {
        f
      }(ExecutionContext.global)

      try {
        Await.result(future, time)
      } catch {
        case e: ExecutionException =>
          throw e.getCause
        case e: TimeoutException =>
          // so, on timeout, we mark the final exit correctly
          fullReset()
          throw e
        case e: Throwable =>
          throw e
      }
    }

    @inline final def time[T](f: => T, timeLimit: Option[Duration] = None): T =
      if (isOutside && timeLimit.isDefined) {
        atMost(process(f), timeLimit.get)
      } else {
        process(f)
      }

    @inline private final def process[T](f: => T) = {
      enter()
      try {
        f
      } catch {
        case e: Throwable =>
          _errorHits += 1
          throw e;
      } finally {
        exit()
      }
    }
    @inline protected[this] final def enter(): Unit = {
      if (isOutside) {
        _lastEntry = System.nanoTime()
        _invocations += 1
      }
      _entries += 1
      _hits += 1
    }

    @inline protected[this] final def exit(): Unit = {
      _entries -= 1
      if (isOutside) {
        _totalTime += (System.nanoTime() - _lastEntry)
      }
    }

    def totalTime: Long = _totalTime

    def hits: Long = _hits

    def errorHits: Long = _errorHits

    /**
      * hits per second
      */
    def speed: Double = {
      if (totalTime > 0)
        (hits.toDouble / totalTime.toDouble) * Math.pow(10, 9)
      else
        Double.NaN
    }

    def report: String = {
      if (_entries != 0) {
        System.err.println("Trying to print a report while inside a measured region")
      }
      name + ": time = " + formatTime(totalTime) + ";  hits: " + _hits +
        (if (hits > 0) f"; speed: $speed%.2f hits/s" else "") +
        (if (_errorHits > 0) "; errorHits: " + _errorHits else "")
    }
  }

}

class Hits {
  var _hits = 0L

  def hit(): Unit = {
    _hits += 1
  }

  def hits = _hits
}