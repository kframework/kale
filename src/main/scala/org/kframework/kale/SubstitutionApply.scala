package org.kframework.kale

import org.kframework.kale.transformer.Unary

object Var {
  def apply(solver: SubstitutionApply)(v: Variable): Term = solver.substitution.get(v).getOrElse(v)
}

class SubstitutionApply(val substitution: Substitution)(implicit env: Environment) extends Unary.Apply() {

  override def processingFunctions = definePartialFunction[Term, this.type]({
    case `Variable` => Var.apply _
  }) orElse env.unaryProcessingFunctions

  override def apply(t: Term): Term = {
    if (t.isGround)
      t
    else if ((t.variables & substitution.boundVariables).nonEmpty) {
      arr(t.label.id)(t)
    }
    else
      t
  }
}

