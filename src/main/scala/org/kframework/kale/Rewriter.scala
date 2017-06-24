package org.kframework.kale

import org.kframework.kale.km.Z3Stuff
import org.kframework.kale.standard.AndOfSubstitutionAndTerms
import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.transformer.Binary

import scala.collection.immutable.TreeSet
import scala.collection.{Set, mutable}

object Rewriter {
  def apply(substitutioner: Substitution => (Term => Term), matcher: MatcherOrUnifier) = new {
    def apply(rules: Set[_ <: Rewrite]): Rewriter = new Rewriter(substitutioner, matcher, rules, matcher.env.asInstanceOf[Environment with Z3Stuff])

    def apply(rule: Term): Rewriter = {
      implicit val e = matcher.env
      apply(Set(rule.moveRewriteToTop))
    }
  }
}

class Rewriter(substitutioner: Substitution => (Term => Term), doMatch: Binary.Apply, val rules: Set[_ <: Rewrite], val env: Environment with Z3Stuff) extends (Term => Stream[Term]) {
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

  val z3 = new z3(env, Seq(Seq()))

  import env._

  def apply(t: Term): Stream[Term] = step(t)

  def step(obj: Term): Stream[Term] = {
    var tries = 0
    val res = (sortedRules.toStream map { r =>
      val m = doMatch(r, obj)
      tries += 1
      m match {
        case Or.set(ands) =>
          val afterSubstitution = env match {
            case env: StandardEnvironment =>
              import env._
              ands.toStream.collect({
                case And.withNext(_: Substitution, Some(Next(next))) => next
              }).headOption.getOrElse(Bottom)
            case _ =>
              val oneGoodSub = (ands collect {
            case s: Substitution => s
            case a: AndOfSubstitutionAndTerms => a.s
          }).headOption


              oneGoodSub.map(substitutioner(_).apply(r._2)).getOrElse(Bottom)
          }
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
    Or(rules.map(r => (doMatch(r, obj), r._2)).flatMap({
      case (Bottom, _) => Set[Term]()
      case (or, rhs) =>
        val res = Or.asSet(or).flatMap(u => {
          env match {
            case environment: StandardEnvironment =>
              val withNext = environment.And.withNext
              val withNext(sub, Some(Next(next))) = u
              Set(next)
            case _ =>
              val (sub, terms) = And.asSubstitutionAndTerms(u)
              val constraints = And(terms.filterNot(_.label == env.Next))
              if (z3.sat(constraints)) {
                Set(And(substitutioner(sub)(rhs), constraints)) // TODO: consider when rhs.predicates is not satisfiable with constraints
              } else {
                Set[Term]()
              }
          }
        })
        res
    }))
  }
}

