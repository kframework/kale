package org.kframework.kale.builtin

import org.kframework.kale.{DomainValue, Environment}
import org.kframework.kale.standard.ReferenceLabel

trait HasID {
  self: Environment =>

  val ID = new ReferenceLabel[Symbol]("ID")(this) {
    override protected[this] def internalInterpret(s: String): Symbol = Symbol(s)
  }

  implicit def toID(s: Symbol): DomainValue[Symbol] = ID(s)
}
