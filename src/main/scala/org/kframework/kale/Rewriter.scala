package org.kframework.kale

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply

import scala.collection.Set

object Rewriter {
  def apply(substitutioner: Substitution => SubstitutionApply, matcher: Binary.Apply, env: Environment)(rules: Set[Rewrite]) =
    new Rewriter(substitutioner, matcher, rules, env)
}

class Rewriter(substitutioner: Substitution => SubstitutionApply, doMatch: Binary.Apply, val rules: Set[Rewrite], val env: Environment) {
  assert(env.isSealed)
  assert(rules != null)

  import env._

  def executionStep(obj: Term): Term = {
    val res = (rules.toStream map { r =>
      val m = doMatch(r._1, obj)
      m match {
        case Or.set(ands) =>
          val oneGoodSub = (ands collect { case s: Substitution => s }).headOption
          val afterSubstitution = oneGoodSub.map(substitutioner(_).apply(r._2)).getOrElse(Bottom)
          if (afterSubstitution != Bottom) {
//            println("   " + r)
//            println("       " + oneGoodSub)
          }
          afterSubstitution
        case Bottom => Bottom
      }
    }).find(_ != Bottom)
    res.getOrElse(obj)
  }

  def searchStep(obj: Term): Term = {
    Or(rules.map(r => (doMatch(r._1, obj), r._2)).flatMap({
      case (Bottom, _) => Set[Term]()
      case (or, rhs) =>
        val substitutions: Set[Substitution] = Or.asSet(or).asInstanceOf[Set[Substitution]]
        substitutions.map(substitutioner).map(_ (rhs))
    }))
  }
}

//object P {
//  def m(left: Term)(matcher: Apply): (Term) => Term = matcher(left, _)
//
//  def ss(t: Term)(implicit substitutioner: Substitution => SubstitutionApply, env: Environment) = {
//    import env._
//    val (subs, terms) = And.unwrap(t)
//    val s = substitutioner(subs)
//    val res = terms.map(s)
//    And(subs, res)
//  }
//
//  def app(rw: Rewrite)(
//    matcher: Apply,
//    substitutioner: Substitution => SubstitutionApply,
//    env: Environment,
//    functionExecution: Term => Term)(obj: Term) = {
//    val Rewrite(l, r) = rw
//    val
//    val afterMatch = m(left)
//
//  }
//}
