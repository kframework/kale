package org.kframework.kale

trait BinaryInfix {
  self: Node2 =>
  override def toString: String =  _1 + " " + label.name + " " + _2
}

trait MemoizedHashCode {
  lazy val cachedHashCode = computeHashCode

  override def hashCode = cachedHashCode

  def computeHashCode: Int
}

