package org.kframework.kale.standard

import org.kframework.kale.{StandardSubstitution, Substitution, SubstitutionApply, Term}
import org.kframework.kale.context.Context1Application
import org.kframework.kale.transformer.Unary

class SubstitutionPieces(env: CurrentEnvironment) {
  import env._

  object Context1 extends Unary.ProcessingFunction[Context1Application, SubstitutionApply] {
    override def f(solver: SubstitutionApply)(t: Context1Application): Term = {
      val recursiveResult = Equality.createBinding(t.hole, solver(t.redex))
      And.substitution(solver.s, recursiveResult) match {
        case subs: Substitution =>
          val innerSolver: SubstitutionApply = StandardSubstitution(solver.processingFunction, env)(subs)

          solver.getVariable(t.contextVar) map innerSolver getOrElse Bottom
        case `Bottom` => Bottom
      }
    }
  }
}
