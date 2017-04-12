package org.kframework.kale.builtin

import org.kframework.kale.Environment
import org.kframework.kale.standard.{PrimordialConstantLabel, ReferenceLabel, Sort}

trait HasGenericToken {
  self: Environment =>

  case class GenericTokenLabel(sort: Sort) extends ReferenceLabel[String]("TOKEN_" + sort.name)(this) {
    override def internalInterpret(s: String): String = s
  }

}