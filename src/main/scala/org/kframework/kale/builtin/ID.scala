package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{DomainValue, Environment, builtin}


trait IdMixin extends kale.IdMixin {
  env: Environment =>

  override val ID = new {
    val Id = new ReferenceLabel[Symbol]("Id@ID") {
      override protected[this] def internalInterpret(s: String): Symbol = Symbol(s)
    }
  }

  implicit val upSymbol = ID.Id
}
