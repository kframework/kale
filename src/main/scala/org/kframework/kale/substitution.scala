package org.kframework.kale

import scala.collection.Map

trait Substitution extends (Term => Term) with Term {
  val boundVariables: Set[Variable]

  def env: Environment

  def get(v: Variable): Option[Term]

  def asMap: Map[Variable, Term]

  lazy val sub = env.substitutionMaker(this)

  def apply(t: Term): Term = sub(t)
}
