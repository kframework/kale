package org.kframework.kale

import org.kframework.kale.util.Named

package object standard {
  def lift(funcName: String, func: Term => Term)(implicit oenv: Environment): Label1 =
    new Named(funcName) with Label1 {
      override def apply(_1: Term): Term = func(_1)
    }

  def lift(funcName: String, func: (Term, Term) => Term)(implicit oenv: Environment): Label2 =
    new Named(funcName) with Label2 {
      override def apply(_1: Term, _2: Term): Term = func(_1, _2)
    }

  def lift(funcName: String, func: Term => Option[Term])(implicit oenv: Environment) =
    new Named(funcName) with FunctionLabel1 {
      override def f(_1: Term): Option[Term] = func(_1)
    }
}
