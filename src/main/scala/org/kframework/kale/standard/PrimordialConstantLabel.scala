package org.kframework.kale.standard

import org.kframework.kale.{Constant, ConstantLabel}

trait PrimordialConstantLabel[T] extends ConstantLabel[T] {
  def apply(v: T): Constant[T] = SimpleConstant(this, v)
}
