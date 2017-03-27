package org.kframework.km

import scala.util.control.ControlThrowable

object unification {

  import term._
  import builtin._

  case class Fail(t1: Term, t2: Term, u: Unifier) extends ControlThrowable

  case class Unifier(subst: Substitution, constraint: Term) {
    assert(constraint.sort == SortBool)
  }

  def unifyTerm(t1: Term, t2: Term, u: Unifier): Unifier = {
    assert(t1.sort == t2.sort)
    if (t1 == t2) u
    else {
      (t1, t2) match {
        case (v1:Variable, _) => unifyVariable(v1, t2, u)
        case (_, v2:Variable) => unifyVariable(v2, t1, u)
        case (_, _) if t1.isFunctional || t2.isFunctional =>
          Unifier(u.subst, BOOL.and(Seq(u.constraint, EQ.of(t1.sort)(Seq(t1,t2)))))
        case (Application(l1,ts1), Application(l2,ts2)) if l1 == l2 && ts1.size == ts2.size =>
          unifyTerms(ts1, ts2, u)
        case _ => throw Fail(t1, t2, u)
      }
    }
  }

  def unifyTerms(ts1: Seq[Term], ts2: Seq[Term], u: Unifier): Unifier = {
    assert(ts1.size == ts2.size)
    (ts1, ts2) match {
      case (t1 +: ts1, t2 +: ts2) =>
        val _u = unifyTerm(t1, t2, u)
        unifyTerms(ts1, ts2, _u)
      case (Seq(), Seq()) => u
      case _ => ???
    }
  }

  def unifyVariable(v1: Variable, t2: Term, u: Unifier): Unifier = {
    assert(v1.sort == t2.sort)
    if (v1 == t2) u
    else if (u.subst.contains(v1)) unifyTerm(u.subst(v1), t2, u)
    else {
      t2 match {
        case v2:Variable if u.subst.contains(v2) =>
          unifyTerm(v1, u.subst(v2), u)
        case _ =>
          if (occur(v1, t2, u)) throw Fail(v1, t2, u)
          else Unifier(u.subst + (v1 -> t2), u.constraint.subst(Map(v1 -> t2))) // TODO: check if enough for constraint
      }
    }
  }

  def occur(v1: Variable, t2: Term, u: Unifier): Boolean = {
    t2 match {
      case v2:Variable =>
        if (v1 == v2) true
        else if (u.subst.contains(v2)) occur(v1, u.subst(v2), u)
        else false
      case Application(_, ts2) => ts2.exists(t => occur(v1,t,u))
      case _ => false
    }
  }

}
