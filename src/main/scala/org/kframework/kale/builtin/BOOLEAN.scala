package org.kframework.kale.builtin

import org.kframework.kale.{DomainValue, Environment, Label, builtin}
import org.kframework.kale.standard.ReferenceLabel

case class BOOLEAN()(implicit penv: Environment) extends Module("BOOLEAN") {
  val Boolean = new ReferenceLabel[Boolean]("Boolean") {
    override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
  }

  override val all: Set[Label] = Set(Boolean)
}

trait importBOOLEAN {
  protected val env: Environment

  val BOOLEAN = builtin.BOOLEAN()(env)

  implicit def toBoolean(b: Boolean): DomainValue[Boolean] = BOOLEAN.Boolean(b)
}
