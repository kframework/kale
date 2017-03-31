package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.context.{Context1Application, Context1ApplicationLabel}
import org.kframework.kale.transformer.Unary
import org.kframework.kale.transformer.Unary.ProcessingFunction

class Context1(implicit env: CurrentEnvironment) extends Unary.ProcessingFunction[Context1Application, SubstitutionApply] {

  import env._

  override def f(solver: SubstitutionApply)(t: Context1Application): Term = {
    val recursiveResult = Equality.binding(t.hole, solver(t.redex))
    And.substitution(solver.s, recursiveResult) match {
      case subs: Substitution =>
        val innerSolver: SubstitutionApply = StandardSubstitution(solver.processingFunction, env)(subs)

        solver.getVariable(t.contextVar) map innerSolver getOrElse Bottom
      case `Bottom` => Bottom
    }
  }
}

object StandardSubstitution {
  def apply(processingFunction: Label => ProcessingFunction[_ <: Term, SubstitutionApply], env: CurrentEnvironment)(s: Substitution): SubstitutionApply = new SubstitutionApply(processingFunction, env)(s)

  def apply(env: CurrentEnvironment): Substitution => SubstitutionApply = {
    assert(env.isSealed)

    import env._

    object Var extends Unary.ProcessingFunction[Variable, SubstitutionApply] {
      def f(solver: SubstitutionApply)(v: Variable) = solver.getVariable(v).getOrElse(v)
    }

    import Unary._

    val processingFunction: PartialFunction[Label, ProcessingFunction[_ <: Term, SubstitutionApply]] = {
      case `Variable` => Var
      case l: Context1ApplicationLabel => new Context1()(env)
    }

    val allProcessingFunction: PartialFunction[Label, ProcessingFunction[_ <: Term, SubstitutionApply]] =
      processingFunction orElse defaultMapping[SubstitutionApply]

    { s: Substitution => new SubstitutionApply({ l: Label => allProcessingFunction(l) }, env)(s) }
  }
}
