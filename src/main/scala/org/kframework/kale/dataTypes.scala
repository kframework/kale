package org.kframework.kale

import org.kframework.kale.builtin._

trait IntMixin extends Mixin {
  val INT: INT
}

trait DoubleMixin extends Mixin {
  val DOUBLE: DOUBLE
}

trait BooleanMixin extends Mixin {
  val BOOLEAN: BOOLEAN
}

trait StringMixin extends Mixin {
  val STRING: STRING
}

trait IdMixin extends Mixin {
  val ID: ID
}