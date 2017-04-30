package org.kframework.kale

import org.kframework.kale.transformer.Unary

object Var  {
  def apply(solver: SubstitutionApply)(v: Variable): Term = solver.substitution.get(v).getOrElse(v)
}

class SubstitutionApply(val substitution: Substitution)(implicit env: Environment) extends Unary.Apply(env) {

  import env._

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case `Variable` => Var.apply _
  }) orElse super.processingFunctions

  override def apply(t: Term): Term = {
    if (t.isGround)
      t
    else {
      arr(t.label.id)(t)
    }
  }
}

