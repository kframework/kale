package org.kframework.kale.context

import org.kframework.kale.{Term, Variable}

trait Context extends Term {
  val contextVar: Variable
  val redex: Term
  override lazy val isPredicate: Boolean = false
}
