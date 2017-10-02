package org.kframework.kale

import org.kframework.kale.builtin._

trait IntMixin extends Mixin {
  env: Environment =>
  val INT: INT
}

trait DoubleMixin extends Mixin {
  env: Environment =>
  val DOUBLE: DOUBLE
}

trait BooleanMixin extends Mixin {
  env: Environment =>
  val BOOLEAN: BOOLEAN
}

trait StringMixin extends Mixin {
  env: Environment =>
  val STRING: STRING
}

trait IdMixin extends Mixin {
  env: Environment =>
  val ID: ID
}