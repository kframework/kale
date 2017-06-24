package org.kframework.kale.builtin

import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{Environment, Sort}

case class GenericTokenLabel(sort: Sort)(implicit override val env: Environment) extends ReferenceLabel[String]("TOKEN_" + sort.name)(env) {
  override protected[this] def internalInterpret(s: String): String = s
}