package org.kframework.kale

import org.kframework.kale.transformer.Unary

import org.kframework.kale.transformer.Unary.ProcessingFunction
import org.kframework.kale.util.Util


class SubstitutionApply(val processingFunction: Label => ProcessingFunction[_ <: Term, SubstitutionApply], val env: Environment)(val s: Substitution) extends Unary.Apply[SubstitutionApply](env) with (Term => Term) {
  import env._

  def getVariable(v: Variable): Option[Term] = s.get(v)

  def fixpoint(t: Term): Term = Util.fixpoint(apply)(t)

  def apply(t: Term): Term =
    if (t.isGround)
      t
    else {
      arr(t.label.id) match {
        case null => Bottom
        case f => f(t)
      }
    }
}