package org.kframework.kale

import org.kframework.kale.transformer.Unary

import scala.collection.Set
import context._

object SubstitutionApply {
  def apply(processingFunction: Label => ProcessingFunction[_ <: Term, SubstitutionApply], env: Environment)(s: Substitution): SubstitutionApply = new SubstitutionApply(processingFunction, env)(s)

  def apply(env: CurrentEnvironment): Substitution => SubstitutionApply = {
    assert(env.isSealed)

    import env._

    object Var extends Unary.ProcessingFunction[Variable, SubstitutionApply] {
      def f(solver: SubstitutionApply)(v: Variable) = solver.getVariable(v).getOrElse(v)
    }

    object Context1 extends Unary.ProcessingFunction[Context1Application, SubstitutionApply] {
      override def f(solver: SubstitutionApply)(t: Context1Application): Term = {
        val recursiveResult = Equality.createBinding(t.hole, solver(t.redex))
        And.substitution(solver.s, recursiveResult) match {
          case subs: Substitution =>
            val innerSolver: SubstitutionApply = SubstitutionApply(solver.processingFunction, solver.env)(subs)

            solver.getVariable(t.contextVar) map innerSolver getOrElse Bottom
          case `Bottom` => Bottom
        }
      }
    }

    import Unary._

    val processingFunction: PartialFunction[Label, ProcessingFunction[_ <: Term, SubstitutionApply]] = {
      case `Variable` => Var
      case l: Context1ApplicationLabel => Context1
    }

    val allProcessingFunction: PartialFunction[Label, ProcessingFunction[_ <: Term, SubstitutionApply]] =
      processingFunction orElse defaultMapping[SubstitutionApply]

    SubstitutionApply({ l: Label => allProcessingFunction(l) }, env)
  }
}

class SubstitutionApply(val processingFunction: Label => ProcessingFunction[_ <: Term, SubstitutionApply], val env: CurrentEnvironment)(val s: Substitution) extends Unary.Apply[SubstitutionApply](env) with (Term => Term) {
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