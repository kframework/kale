package org.kframework.kale.builtin

import org.kframework.kale.Environment
import org.kframework.kale.standard.ReferenceLabel

trait HasSTRING {
  self: Environment =>

  val STRING = new ReferenceLabel[String]("String")(this) {
    override protected[this] def internalInterpret(s: String): String = s
  }
}
