package org.kframework.kale

import org.kframework.kale.transformer.Unary

object Var {
  def apply(solver: SubstitutionApply)(v: Variable): Term = solver.substitution.get(v).getOrElse(v)
}

class SubstitutionApply(val substitution: Substitution)(implicit penv: Environment) extends Unary.Apply() {

  import penv._

  def ExistsSub(solver: SubstitutionApply)(v: Exists): Term =
    substitutionMaker(solver.substitution.remove(v.v))(v.p)

  override def processingFunctions = definePartialFunction[Term, this.type]({
    case `Variable` => Var.apply _
    case Exists => ExistsSub _
  }) orElse env.unaryProcessingFunctions

  override def apply(t: Term): Term = {
    if (t.isGround) {
      t
    } else if ((t.variables & substitution.boundVariables).nonEmpty) {
      arr(t.label.id)(t)
    } else {
      t
    }
  }
}

