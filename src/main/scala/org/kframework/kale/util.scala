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

  def variables(t: Term): Set[Variable] = t match {
    case v: Variable => Set(v)
    case Node(_, cs) => (cs flatMap variables).toSet
    case _ => Set()
  }
}

object unreachable {
  def apply() = throw new AssertionError("unreachable")
}
