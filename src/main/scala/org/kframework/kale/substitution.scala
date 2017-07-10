package org.kframework.kale

trait Substitution extends (Term => Term) with Term {
  val boundVariables: Set[Variable]

  def env: Environment

  def get(v: Variable): Option[Term]

  def remove(v: Variable): Substitution

  def asMap: Map[Variable, Term]

  lazy val sub = env.substitutionMaker(this)

  def apply(t: Term): Term = sub(t)
}
