package org.kframework.km

import scala.util.control.ControlThrowable

/*
  Implemented the algorithm presented in:
    Peter Norvig. Correcting a Widespread Error in Unification Algorithms. SPE. 1991.
    http://dx.doi.org/10.1002/spe.4380210208
*/
object unification {

  import term._

  case class Fail(t1: Term, t2: Term, u: Unifier) extends ControlThrowable

  case class Unifier(subst: Substitution, constraint: Seq[(Term,Term)])

  def unify(t1: Term, t2: Term): Option[Unifier] = {
    val u = Unifier(Map(), Seq())
    try {
      Some(unifyTerm(t1, t2, u))
    } catch {
      case e:Fail => None
    }
  }

  @throws(classOf[Fail])
  def unifyTerm(t1: Term, t2: Term, u: Unifier): Unifier = {
    assert(t1.sort == t2.sort)
    if (t1 == t2) u
    else {
      (t1, t2) match {
        case (v1:Variable, _) => unifyVariable(v1, t2, u)
        case (_, v2:Variable) => unifyVariable(v2, t1, u)
        case (_, _) if t1.isFunctional || t2.isFunctional =>
          Unifier(u.subst, u.constraint :+ (t1,t2))
        case (Application(l1,ts1), Application(l2,ts2)) if l1 == l2 && ts1.size == ts2.size =>
          unifyTerms(ts1, ts2, u)
        case _ => throw Fail(t1, t2, u)
      }
    }
  }

  @throws(classOf[Fail])
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

  @throws(classOf[Fail])
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
          else Unifier(u.subst + (v1 -> t2), subst(u.constraint, Map(v1 -> t2))) // TODO: check if enough for constraint
      }
    }
  }

  def occur(v1: Variable, t2: Term, u: Unifier): Boolean = {
    t2 match {
      case v2:Variable =>
        if (v1 == v2) true
        else if (u.subst.contains(v2)) occur(v1, u.subst(v2), u)
        else false
      case Application(_, ts2) => ts2.exists(occur(v1,_,u))
      case _ => false
    }
  }

  def subst(constraint: Seq[(Term,Term)], subst: Substitution): Seq[(Term,Term)] = {
    constraint.map({case (t1,t2) => (t1.subst(subst), t2.subst(subst))})
  }

}
