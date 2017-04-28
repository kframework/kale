package org.kframework.kale

import org.kframework.kale.transformer.Unary

object Var extends Unary.ProcessingFunction[SubstitutionApply] {
  type Element = Variable

  def f(solver: SubstitutionApply)(v: Variable): Term = solver.substitution.get(v).getOrElse(v)
}

class SubstitutionApply(val substitution: Substitution)(implicit env: Environment) extends Unary.Apply(env) {

  import env._

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case `Variable` => Var
  }) orElse super.processingFunctions

  override def apply(t: Term): Term = {
    if (t.isGround)
      t
    else {
      arr(t.label.id)(t)
    }
  }
}

