package kale

import scala.collection.IterableLike
import scala.collection.mutable.Builder

trait MemoizedHashCode {
  lazy val cachedHashCode = computeHashCode
  override def hashCode = cachedHashCode
  def computeHashCode: Int
}
