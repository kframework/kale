package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class BOOLEAN()(implicit env: Environment) extends Module {
  val Boolean = new ReferenceLabel[Boolean]("Boolean") {
    override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
  }

  val not = PrimitiveFunction1[Boolean]("notBool_", Boolean, x => !x)
  val and = PrimitiveFunction2[Boolean]("_andBool_", Boolean, (x, y) => x && y)
  val True = Boolean(true)
  val False = Boolean(false)
}

trait importBOOLEAN {
  protected val env: Environment

  val BOOLEAN = builtin.BOOLEAN()(env)

  implicit def toBoolean(b: Boolean): DomainValue[Boolean] = BOOLEAN.Boolean(b)
}
