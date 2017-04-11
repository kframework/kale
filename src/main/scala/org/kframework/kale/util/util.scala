package org.kframework.kale.util

import org.kframework.kale.standard.Rewrite
import org.kframework.kale.{Environment, Node, Term, Variable}

object Util {
  def fixpoint[T](f: T => T): (T => T) = {
    { t: T =>
      val after = f(t)
      if (after != t)
        fixpoint(f)(after)
      else
        after
    }
  }

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
}
