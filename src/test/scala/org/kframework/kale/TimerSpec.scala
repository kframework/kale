package org.kframework.kale

import org.kframework.kale.util.timer
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, FreeSpec}

import concurrent.duration._
import scala.concurrent.TimeoutException

class TimerSpec extends FreeSpec with BeforeAndAfter with BeforeAndAfterAll {

  before(timer.fullReset())

  override def afterAll {
    timer.fullReset()
  }

  "timer" in {
    val t = timer("foo")

    def time(x: Int): Unit = {
      t.time({
        Thread.sleep(100)
        if (x > 0)
          time(x - 1)
      }, None)
    }
    time(3)
    assert(t.totalTime > 400.millis && t.totalTime < 450.millis)
    println(timer.fullReport())
  }

  "exception in timer" in {
    val t = timer("foo")

    def time(x: Int): Unit = {
      t.time({
        Thread.sleep(100)
        if (x > 0)
          time(x - 1)
        else
          throw new AssertionError("foo")
      }, None)
    }
    try {
      time(3)
    } catch {
      case e: AssertionError =>
        assert(t.totalTime > 400.millis && t.totalTime < 450.millis)
    }
  }

  "timeout" in {
    val t = timer("foo")

    def time(x: Int): Unit = {
      t.time({
        Thread.sleep(100)
        if (x > 0)
          time(x - 1)
        else
          throw new AssertionError("foo")
      }, Some(200.millis))
    }
    try {
      time(10)
    } catch {
      case e: TimeoutException =>
        assert(t.totalTime > 200.millis && t.totalTime < 250.millis)
    }
  }

  "test gradual reset" in {
    val t = timer("foo")

    def time(x: Int): Unit = {
      t.time({
        timer("bar").time {
          timer("buz").time {
            Thread.sleep(100)
          }
          if (x > 0)
            time(x - 1)
          else
            throw new AssertionError("foo")
        }
      }, Some(210.millis))
    }
    try {
      time(10)
    } catch {
      case e: TimeoutException =>
        assert(timer("foo").totalTime.toMillis.millis > 200.millis && timer("foo").totalTime.toMillis.millis < 270.millis)
        assert(timer("bar").totalTime.toMillis.millis > 200.millis && timer("bar").totalTime.toMillis.millis < 270.millis)
        assert(timer("buz").totalTime.toMillis.millis > 200.millis && timer("buz").totalTime.toMillis.millis < 270.millis)
    }
  }
}
