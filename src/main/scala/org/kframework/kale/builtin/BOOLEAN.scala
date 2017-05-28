package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class BOOLEAN()(implicit penv: Environment) extends Module("BOOLEAN") {
  val Boolean = new ReferenceLabel[Boolean]("Boolean") {
    override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
  }

  val not = PrimitiveFunction1[Boolean]("notBool_", Boolean, x => !x)
  val and = PrimitiveFunction2[Boolean]("_andBool_", Boolean, (x, y) => x && y)

  override val all: Set[Label] = Set(Boolean, not)
}

trait importBOOLEAN {
  protected val env: Environment

  val BOOLEAN = builtin.BOOLEAN()(env)

  implicit def toBoolean(b: Boolean): DomainValue[Boolean] = BOOLEAN.Boolean(b)
}
