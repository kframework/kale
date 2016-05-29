package org.kframework.kale

import org.kframework.kale.transformer.Binary

import scala.collection.Set

object Rewriter {
  def apply(substitutioner: Substitution => SubstitutionApplication, matcher: Binary.Application)(rules: Set[Rewrite]) =
    new Rewriter(substitutioner, matcher, rules)
}

class Rewriter(substitutioner: Substitution => SubstitutionApplication, matcher: Binary.Application, rules: Set[Rewrite]) {
  def executionStep(obj: Term): Term = {
    rules.toStream.map(r => (matcher(r._1, obj), r._2)).find(_._1 != Bottom) match {
      case Some((substitutions, rhs)) =>
        val oneSubstitutuion = Or.unwrap(substitutions).head.asInstanceOf[Substitution]
        substitutioner(oneSubstitutuion).apply(rhs)
      case None => Bottom
    }
  }

  def searchStep(obj: Term): Term = {
    Or(rules.map(r => (matcher(r._1, obj), r._2)).flatMap({
      case (Bottom, _) => Set[Term]()
      case (or, rhs) =>
        val substitutions: Set[Substitution] = Or.unwrap(or).asInstanceOf[Set[Substitution]]
        substitutions.map(substitutioner).map(_ (rhs))
    }))
  }
}