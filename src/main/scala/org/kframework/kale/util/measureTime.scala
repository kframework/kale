package org.kframework.kale.util

object measureTime {

  class Reentrant(name: String) {
    var entries = 0
    var totalTime = 0L
    var lastEntry = 0L
    var hits = 0L

    def enter(): Unit = {
      if (entries == 0) {
        lastEntry = System.nanoTime()
      }
      entries += 1
      hits += 1
    }

    def exit(): Unit = {
      entries -= 1
      if (entries == 0) {
        totalTime += (System.nanoTime() - lastEntry)
      }
    }
  }

  private val rs = collection.mutable.Map[String, Reentrant]()

  def apply[T](name: String, active: Boolean = true)(f: => T): T = if (active) {
    enter(name)
    val res = f
    exit(name)
    res
  } else {
    f
  }

  def enter(name: String) = {
    val r = rs.getOrElseUpdate(name, new Reentrant(name))
    r.enter()
  }

  def exit(name: String): Unit = {
    val r = rs(name)
    r.exit()
  }

  def totalTime(name: String): Option[Long] = {
    rs.get(name).map(_.totalTime)
  }

  def report(name: String): Option[String] = {
    rs.get(name) map { r => name + "\n  time: " + formatTime(r.totalTime) + "\n  hits: " + r.hits }
  }

  def reset(name: String) = {
    rs.remove(name)
  }

  def hits(name: String): Option[Long] = {
    rs.get(name).map(_.hits)
  }

  def formatTime(l: Long) = {
    (l / 1000000) + "ms"
  }

  def fullReport(): String = {
    rs.keys flatMap report mkString "\n\n"
  }

  def fullReset(): Unit = {
    rs.clear()
  }
}
