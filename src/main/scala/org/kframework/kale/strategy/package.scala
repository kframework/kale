package org.kframework.kale

import org.kframework.kale.standard.SingleSortedMatcher

package object strategy {

  // only works for ground obj
  def orElseTerm(solver: SingleSortedMatcher)(orElse: Term, obj: Term): Term = {
    import solver.env._
    val STRATEGY.orElse(theThen, theElse) = orElse

    obj.asOr map { t =>
      val thenSol = unify(theThen, t)
       thenSol match {
        case Bottom => unify(theElse, t)
        case other => other
      }
    }
  }

  def composeTerm(solver: SingleSortedMatcher)(composed: Term, obj: Term): Term = {
    import solver.env._
    import STRATEGY._
    val compose(f, g) = composed
    val matchG = unify(g, obj)
    val takeRelevantFromGMatch = nextIsNow(onlyNext(matchG))
    val matchF = unify(f, takeRelevantFromGMatch)

    matchF
  }

  def repeatTerm(solver: SingleSortedMatcher)(fp: Term, obj: Term): Term = {
    import solver.env._
    import STRATEGY._
    val repeat(f) = fp
    val someVar = Variable.freshVariable()
    val sol = solver(orElse(f, someVar), obj)
    sol.asOr map {
      case And.withNext(p, Some(Next(t))) =>
        if (p.contains(someVar)) {
          Next(t)
        } else {
          solver(fp, t) // TODO: pass in the remaining predicates
        }
    }
  }

  def fixpointTerm(solver: SingleSortedMatcher)(fp: Term, obj: Term): Term = {
    import solver.env._
    import STRATEGY._
    val fixpoint(f) = fp
    solver(f, obj) match {
      case Bottom => Bottom
      case Next(`obj`) => Next(obj)
      case res => solver(fp, And.nextIsNow(res))
    }
  }
}
