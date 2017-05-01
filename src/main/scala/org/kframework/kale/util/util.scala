package org.kframework.kale.util

import org.kframework.kale.standard
import org.kframework.kale.standard.Rewrite
import org.kframework.kale.{Environment, Node, Term, Variable}

object fixpoint {
  def apply[T](f: T => T): (T => T) = {
    { t: T =>
      val after = f(t)
      if (after != t)
        apply(f)(after)
      else
        after
    }
  }
}

object Util {


  def variables(t: Term): Set[Variable] = t match {
    case v: Variable => Set(v)
    case Node(_, cs) => (cs flatMap variables).toSet
    case _ => Set()
  }

  def toRewriteLHS(t: Term): Term = t match {
    case Rewrite(l, _) => l
    case n: Node => n.copy(n.children map toRewriteLHS toSeq)
    case _ => t
  }

  def toRewriteRHS(t: Term): Term = t match {
    case Rewrite(_, r) => r
    case n: Node => n.copy(n.children map toRewriteRHS toSeq)
    case _ => t
  }

  def moveRewriteSymbolToTop(t: Term)(implicit env: Environment): Rewrite = env.Rewrite(toRewriteLHS(t), toRewriteRHS((t))).asInstanceOf[Rewrite]

  def contains(t: Term, subterm: Term): Boolean = if (t == subterm) true else t.children.exists(contains(_, subterm))

  def containsInConstructor(t: Term, subterm: Term): Boolean =
    if (t == subterm) true
    else if (!t.label.isInstanceOf[standard.Constructor]) false
    else t.children.exists(containsInConstructor(_, subterm))
}
