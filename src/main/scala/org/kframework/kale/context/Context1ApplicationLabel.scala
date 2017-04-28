package org.kframework.kale.context

import org.kframework.kale.context.anywhere.{AnywhereContextApplication, ContextContentVariable}
import org.kframework.kale.{Label2, Term, Variable}

trait Context1ApplicationLabel extends Label2 with ContextLabel {
  def hole(x: Variable): ContextContentVariable

  override def unapply(t: Term): Option[(Variable, Term)] = t match {
    case n: AnywhereContextApplication if n.label == this => Some(n._1, n._2)
    case _ => None
  }
}
