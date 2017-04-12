package org.kframework.kale.builtin

import org.kframework.kale.Environment
import org.kframework.kale.standard.ReferenceLabel

trait HasID {
  self: Environment =>

  val ID = new ReferenceLabel[String]("ID")(this) {
    override def internalInterpret(s: String): String = s
  }
}
