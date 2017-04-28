package org.kframework.kale.builtin

import org.kframework.kale.Environment
import org.kframework.kale.standard.ReferenceLabel

trait HasBOOLEAN {
  self: Environment =>

  val BOOLEAN = new ReferenceLabel[Boolean]("Boolean")(this) {
    override protected[this] def internalInterpret(s: String): Boolean = s.toBoolean
  }
}
