package org.kframework.kale

import org.kframework.kale.util.Named

package object standard {
  def lift(funcName: String, func: Term => Term)(implicit oenv: Environment): Label1 =
    new Named(funcName) with Label1 with FunctionLabel {
      override def apply(_1: Term): Term = func(_1)

      override val isPredicate = Some(false)
    }

  def lift(funcName: String, func: (Term, Term) => Term)(implicit oenv: Environment): Label2 =
    new Named(funcName) with Label2 with FunctionLabel {
      override def apply(_1: Term, _2: Term): Term = func(_1, _2)

      override lazy val isPredicate = Some(false)
    }

  def lift(funcName: String, func: Term => Option[Term], isPred: Option[Boolean])(implicit oenv: Environment) =
    new Named(funcName) with FunctionLabel1 {
      override def f(_1: Term): Option[Term] = func(_1)

      override lazy val isPredicate = isPred
    }
}
