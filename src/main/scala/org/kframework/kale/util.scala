package org.kframework.kale

trait MemoizedHashCode {
  lazy val cachedHashCode = computeHashCode

  override def hashCode = cachedHashCode

  def computeHashCode: Int
}

object Util {
  def bottomize(_1: Term)(f: => Term): Term = {
    if (Bottom == _1)
      Bottom
    else
      f
  }

  def bottomize(_1: Term, _2: Term)(f: => Term): Term = {
    if (Bottom == _1 || Bottom == _2)
      Bottom
    else
      f
  }

  def fixpoint[T](f: T => T): (T => T) = {
    { t: T =>
      val after = f(t)
      if (after != t)
        fixpoint(f)(after)
      else
        after
    }
  }
}

object unreachable {
  def apply() = throw new AssertionError("unreachable")
}
