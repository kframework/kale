package org.kframework.kale.context

import org.kframework.kale.standard.{CurrentEnvironment, SubstitutionWithContext}
import org.kframework.kale.transformer.Unary
import org.kframework.kale.{Substitution, SubstitutionApply, Term}

class Context1ProcessingFunction(implicit env: CurrentEnvironment) extends Unary.ProcessingFunction[SubstitutionApply] {
  type Element = Context1Application

  import env._

  override def f(solver: SubstitutionApply)(t: Context1Application): Term = {
    val recursiveResult = Equality.binding(t.hole, solver(t.redex))
    And.substitution(solver.substitution, recursiveResult) match {
      case subs: Substitution =>
        val innerSolver = new SubstitutionWithContext(subs)(env)

        solver.substitution.get(t.contextVar) map innerSolver getOrElse Bottom
      case `Bottom` => Bottom
    }
  }
}

class PatternContextProcessingFunction(implicit env: CurrentEnvironment) extends Unary.ProcessingFunction[SubstitutionApply] {
  type Element = PatternContextApplication

  import env._

  override def f(solver: SubstitutionApply)(t: PatternContextApplication): Term = {
    val recursiveResult = Equality.binding(Hole, solver(t.redex))
    And.substitution(solver.substitution, recursiveResult) match {
      case subs: Substitution =>
        val innerSolver = new SubstitutionWithContext(subs)(env)

        solver.substitution.get(t.contextVar) map innerSolver getOrElse Bottom
      case `Bottom` => Bottom
    }
  }
}
