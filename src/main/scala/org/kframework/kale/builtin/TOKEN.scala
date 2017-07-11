package org.kframework.kale.builtin

import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{Environment, Sort}

case class TOKEN(override val name: String, sort: Sort)(implicit override val env: Environment) extends ReferenceLabel[String](name) {
  override protected[this] def internalInterpret(s: String): String = s
}
