package org.kframework.kale

import org.kframework.kale.standard.SingleSortedMatcher

package object strategy {
  def orElseTerm(solver: SingleSortedMatcher)(orElse: Term, obj: Term): Term = {
    import solver.env._
    val STRATEGY.orElse(pattern, theElse) = orElse
    solver(pattern, obj) match {
      case Bottom => unify(theElse, obj)
      case other => other
    }
  }

  def functionReferenceTerm(solver: SingleSortedMatcher)(functionReference: Term, obj: Term) = {
    import solver.env._
    val STRATEGY.unappliedFunctionReference(v: Variable, inside) = functionReference
    Equality.binding(v, obj)(inside)
  }

  def composeTerm(solver: SingleSortedMatcher)(composed: Term, obj: Term) = {
    import solver.env._
    import STRATEGY._
    val compose(f, g) = composed
    val matchG = unify(g, obj)
    val takeRelevantFromGMatch = nextIsNow(onlyNext(matchG))
    val matchF = unify(f, takeRelevantFromGMatch)

    matchF
  }

  def repeatTerm(solver: SingleSortedMatcher)(fp: Term, obj: Term) = {
    import solver.env._
    import STRATEGY._
    val repeat(f) = fp
    solver(f, obj) match {
      case Bottom => Next(obj)
      case res => solver(fp, And.nextIsNow(res))
    }
  }

  def fixpointTerm(solver: SingleSortedMatcher)(fp: Term, obj: Term) = {
    import solver.env._
    import STRATEGY._
    val fixpoint(f) = fp
    solver(f, obj) match {
      case Next(`obj`) => Next(obj)
      case res => solver(fp, And.nextIsNow(res))
    }
  }
}
