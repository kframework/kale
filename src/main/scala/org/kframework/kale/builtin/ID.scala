package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{DomainValue, Environment, Up, builtin}


trait IdMixin extends kale.IdMixin {
  env: Environment =>

  override val ID = new {
    val Id = define[Symbol]("Id@ID")(Symbol(_))
  }

  implicit val upSymbol = ID.Id
}
