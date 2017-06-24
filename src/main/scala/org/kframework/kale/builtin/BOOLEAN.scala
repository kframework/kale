package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class BOOLEAN()(implicit env: Environment) {
  val Boolean = new ReferenceLabel[Boolean]("Boolean") {
    override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
  }

  val not = PrimitiveFunction1[Boolean]("notBool_", Boolean, x => !x)
  val and = PrimitiveFunction2[Boolean]("_andBool_", Boolean, (x, y) => x && y)
  val True = Boolean(true)
  val False = Boolean(false)
}

trait BooleanMixin extends kale.BooleanMixin with Environment {

  val BOOLEAN = builtin.BOOLEAN()

  implicit def toBoolean(b: Boolean): DomainValue[Boolean] = BOOLEAN.Boolean(b)
}
