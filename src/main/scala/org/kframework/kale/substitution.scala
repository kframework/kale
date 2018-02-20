package org.kframework.kale

trait Substitution extends (Term => Term) with Term {
  val boundVariables: Set[Variable]

  def env: Environment

  def get(v: Variable): Option[Term]

  def filter(f: Variable => Boolean): Substitution

  def remove(v: Variable): Substitution = filter(_ != v)

  def asMap: Map[Variable, Term]

  lazy val sub = env.substitutionMaker(this)

  def apply(t: Term): Term = sub(t)


}
