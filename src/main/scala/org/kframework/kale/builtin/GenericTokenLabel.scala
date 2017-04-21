package org.kframework.kale.builtin

import org.kframework.kale.{Environment, Sort}
import org.kframework.kale.standard.ReferenceLabel

case class GenericTokenLabel(sort: Sort)(implicit override val env: Environment) extends ReferenceLabel[String]("TOKEN_" + sort.name)(env)