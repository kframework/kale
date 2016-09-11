package org.kframework.kale

trait MemoizedHashCode {
  lazy val cachedHashCode = computeHashCode

  override def hashCode = cachedHashCode

  def computeHashCode: Int
}

object Util {
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
