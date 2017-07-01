package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{DomainValue, Environment, builtin}

case class ID()(implicit protected val penv: Environment) {
  val Id = new ReferenceLabel[Symbol]("Id@ID")(penv) {
    override protected[this] def internalInterpret(s: String): Symbol = Symbol(s)
  }
}

trait IdMixin extends kale.IdMixin {
  protected val env: Environment

  val ID = builtin.ID()(env)

  implicit def toID(s: Symbol): DomainValue[Symbol] = ID.Id(s)

}
