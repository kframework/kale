package org.kframework.kale

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply

import scala.collection.immutable.TreeSet
import scala.collection.{Set, mutable}

object Rewriter {
  def apply(substitutioner: Substitution => SubstitutionApply, matcher: Binary.Apply, env: Environment)(rules: Set[Rewrite]) =
    new Rewriter(substitutioner, matcher, rules, env)
}

class Rewriter(substitutioner: Substitution => SubstitutionApply, doMatch: Binary.Apply, val rules: Set[Rewrite], val env: Environment) {
  assert(env.isSealed)
  assert(rules != null)

  val ruleHits = mutable.Map[Rewrite, Int]()

  for (r <- rules)
    ruleHits += (r -> 0)

  var sortedRules = TreeSet[Rewrite]()({ (r1, r2) =>
    if (r1 == r2)
      0
    else {
      val p = ruleHits(r2) - ruleHits(r1)
      if (p != 0)
        p
      else {
        val id = System.identityHashCode(r1) - System.identityHashCode(r2)
        if (id > 0)
          1
        else
          -1
      }
    }
  })

  sortedRules ++= rules

  import env._

  def step(obj: Term): Stream[Term] = {
    var tries = 0
    val res = (sortedRules.toStream map { r =>
      val m = doMatch(r._1, obj)
      tries += 1
      m match {
        case Or.set(ands) =>
          val oneGoodSub = (ands collect { case s: Substitution => s }).headOption
          val afterSubstitution = oneGoodSub.map(substitutioner(_).apply(r._2)).getOrElse(Bottom)
          //          if (afterSubstitution != Bottom) {
          //            println("   " + r)
          //            println("       " + oneGoodSub)
          //          }
          if (afterSubstitution != Bottom) {
            val prev = ruleHits(r)
            sortedRules -= r
            ruleHits.update(r, prev + 1)
            sortedRules += r
          }
          afterSubstitution
        case Bottom => Bottom
      }
    }).filterNot(_ == Bottom)
    res
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
