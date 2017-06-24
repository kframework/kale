package org.kframework.kale.builtin

import org.kframework.kale.{DomainValue, Environment, Label}
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.builtin

case class ID()(implicit protected val penv: Environment) {
  val Id = new ReferenceLabel[Symbol]("Id")(penv) {
    override protected[this] def internalInterpret(s: String): Symbol = Symbol(s)
  }
}

trait importID {
  protected val env: Environment

  val ID = builtin.ID()(env)

  implicit def toID(s: Symbol): DomainValue[Symbol] = ID.Id(s)

}
