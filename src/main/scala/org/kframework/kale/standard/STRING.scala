package org.kframework.kale.standard

import org.kframework.kale.Environment

trait HasSTRING {
  self: Environment =>

  val STRING = new ReferenceLabel[String]("String")(this) {
    override def internalInterpret(s: String): String = s
  }
}
