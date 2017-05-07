package org.kframework.kale.builtin

import org.kframework.kale.{DomainValue, Environment}
import org.kframework.kale.standard.ReferenceLabel

trait HasSTRING {
  self: Environment =>

  val STRING = new ReferenceLabel[String]("String")(this) {
    override protected[this] def internalInterpret(s: String): String = s
  }

  implicit def toSTRING(s: String): DomainValue[String] = STRING(s)
}
