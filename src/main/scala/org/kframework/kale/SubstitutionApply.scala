package org.kframework.kale

import org.kframework.kale.transformer.Unary

object Var {
  def apply(solver: SubstitutionApply)(v: Variable): Term = solver.substitution.get(v).getOrElse(v)
}

object Eql {
  def apply(solver: SubstitutionApply)(e: Equals): Term = {
    val sub = solver.substitution
    e._1 match {
      case v: Variable =>
        val rhs = sub.get(v) match {
          case Some(value) if value != e._2 =>
            solver.env.unify(value, e._2)
          case _ =>
            e._2
        }
        e.copy(v, sub(rhs))
      case _ =>
        e.copy(sub(e._1), sub(e._2))
    }
  }
}

class SubstitutionApply(val substitution: Substitution)(implicit penv: Environment) extends Unary.Apply() {

  import penv._

  def ExistsSub(solver: SubstitutionApply)(v: Exists): Term =
    substitutionMaker(solver.substitution.remove(v.v))(v.p)

  override def processingFunctions = definePartialFunction[Term, this.type]({
    case `Equality` => Eql.apply _
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

