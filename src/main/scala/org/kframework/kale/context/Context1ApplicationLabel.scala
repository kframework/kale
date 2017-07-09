package org.kframework.kale.context

import org.kframework.kale.context.anywhere.ContextContentVariable
import org.kframework.kale.{Label2, Term, Variable}

trait Context1ApplicationLabel extends Label2 with ContextLabel {
  def hole(x: Variable): ContextContentVariable
}
