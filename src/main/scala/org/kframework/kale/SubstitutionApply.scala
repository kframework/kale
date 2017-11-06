package org.kframework.kale

import org.kframework.kale.transformer.Unary
import org.kframework.kale.transformer.Unary.Apply

object Var {
  def apply(solver: SubstitutionApply)(v: Variable): Term = solver.substitution.get(v).getOrElse(v)
}

class SubstitutionApply(val substitution: Substitution)(implicit penv: Environment) extends Unary.Apply() {

  import penv._

  def ExistsSub(solver: SubstitutionApply)(v: Exists): Term =
    substitutionMaker(solver.substitution.remove(v.v))(v.p)

  def HSPF(solver: SubstitutionApply)(t: FreeNode1): Term = {
    t.copy(solver(t._1))
  }

  override def processingFunctions = definePartialFunction[Term, this.type]({
    case `Variable` => Var.apply _
    case Exists => ExistsSub _
    case l: HasSortPredicateFunction => HSPF _
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

