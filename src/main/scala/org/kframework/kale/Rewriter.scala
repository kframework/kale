package org.kframework.kale

import org.kframework.kale.km.{MultisortedMixing, Z3Mixin}
import org.kframework.kale.standard.StandardEnvironment

import scala.collection.immutable.TreeSet
import scala.collection.mutable

@Deprecated
object Rewriter {
  def apply(env: StandardEnvironment) = new {
    def apply(rules: Set[_ <: Term]): Rewriter = new Rewriter(env)(rules)
  }
}

@Deprecated
class Rewriter(val env: StandardEnvironment)(val inputRules: Set[_ <: Term]) extends (Term => Option[Term]) {
  assert(env.isSealed)
  assert(inputRules != null)

  private def lowerForAll(keep: Set[Variable]): Term => Term = {
    case v: Variable =>
      if (keep.contains(v))
        v
      else
        env.ForAll(v, v)

    case t =>
      val newKeep: Set[Variable] = t.variables filter (v => t.children.count(_.contains(v)) > 1)
      val kill = newKeep &~ keep

      val newLowerForAll = lowerForAll(keep | newKeep)
      val solvedChildren = t map0 newLowerForAll

      val withKill: Term = kill.foldLeft(solvedChildren) { (tt, v) => env.ForAll(v, tt) }
      withKill
  }


  val rules = inputRules map lowerForAll(Set())

  println(rules.mkString("\n"))

  val ruleHits = mutable.Map[Term, Int]()

  for (r <- rules)
    ruleHits += (r -> (Math.random() * 20).toInt)

  var sortedRules = TreeSet[Term]()({ (r1, r2) =>
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

  //  def index(t: Term) = t findTD {
  //    kseq(ForAll(_, ))
  //  }

  sortedRules ++= rules

  // TODO: clean this
  val z3 = env match {
    case e: Environment with MultisortedMixing with Z3Mixin => new z3(e, Seq(Seq()))
    case _ => null
  }

  import env._

  //  val kseq = label("#KSequence").asInstanceOf[Label2]
  //  val topCell = label("<T>").asInstanceOf[Label2]
  //  val kCell = label("<k>").asInstanceOf[Label1]
  //
  //  def indexTerm(t: Term): Option[Label] = (t findTD {
  //    case kseq(a, _) => true
  //    case _ => false
  //  }).map(_.asInstanceOf[Node2]._1.label)
  //
  //  def index(t: Term): Option[Label] = t match {
  //    case topCell(x, _) => index(x)
  //    case kCell(x) => index(x)
  //    case env.ForAll(_, x) => index(x)
  //    case kseq(x, _) => index(x)
  //    case Rewrite(x, _) => index(x)
  //    case v: Variable => None
  //    case x => Some(x.label)
  //  }
  //
  //  val indexedRules = sortedRules.groupBy(index).filterKeys(_.isDefined).map({ case (k, v) => (k.get, v) })

  import env._

  def apply(t: Term): Option[Term] = step(t)

  var indexHits = 0
  var sortedHits = 0

  def step(obj: Term): Option[Term] = {
    var tries = 0

    //    val indexed = indexTerm(obj).flatMap(indexedRules.get).getOrElse(Set())
    //
    //    val res0 = indexed.view map { r =>
    //      val m = unify(r, obj)
    //      tries += 1
    //      m match {
    //        case Or.set(ands) =>
    //          val afterSubstitution = env match {
    //            case env: StandardEnvironment =>
    //              import env._
    //              ands.view.collect({
    //                case And.withNext(_: Substitution, Some(Next(next))) => next
    //              }).headOption.getOrElse(Bottom)
    //          }
    //          if (afterSubstitution != Bottom) {
    //            indexHits += 1
    //          }
    //          afterSubstitution
    //        case Bottom => Bottom
    //      }
    //    }

    val res = sortedRules.view map { r =>
      val m = unify(r, obj)
      tries += 1
      m match {
        case Or.set(ands) =>
          val afterSubstitution = env match {
            case env: StandardEnvironment =>
              import env._
              ands.view.collect({
                case And.withNext(_: Substitution, Some(Next(next))) => next
              }).headOption.getOrElse(Bottom)
          }
          if (afterSubstitution != Bottom) {
            val prev = ruleHits(r)
            sortedRules -= r
            ruleHits.update(r, prev + (Math.random() * 3).toInt)
            sortedRules += r
            sortedHits += 1
          }
          afterSubstitution
        case Bottom => Bottom
      }
    }

    //    res0.find(_ != Bottom).orElse()
    res.find(_ != Bottom)
  }

  def searchStep(obj: Term): Term = {
    val unificationRes: Set[Term] = rules.map(r => unify(r, obj))
    Or(unificationRes.flatMap({
      case Bottom => Set[Term]()
      case or =>
        val res = Or.asSet(or).flatMap(u => {
          val And.SPN(sub, pred@And.set(unresolvedConstraints), Next(next)) = u

          val allConstraints = And(sub, pred)

          if (unresolvedConstraints.nonEmpty && env.isInstanceOf[Z3Mixin] && !z3.sat(allConstraints)) {
            Set[Term]()
          } else {
            Set(And(next, allConstraints))
          }
        })
        res
    }))
  }
}

