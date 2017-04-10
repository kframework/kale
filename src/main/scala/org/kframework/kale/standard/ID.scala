package org.kframework.kale.standard

import org.kframework.kale.Environment

trait HasID {
  self: Environment =>

  val ID = new ReferenceLabel[String]("ID")(this) {
    override def internalInterpret(s: String): String = s
  }
}
