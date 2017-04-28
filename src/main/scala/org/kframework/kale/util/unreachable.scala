package org.kframework.kale.util

object unreachable {
  def apply() = throw new AssertionError("unreachable")
}
