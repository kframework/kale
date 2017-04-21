package org.kframework.kale.builtin

import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{Environment, FunctionLabel2, Term}

trait HasINT {
  self: Environment =>

  val INT = new ReferenceLabel[Int]("Int")(this)
}

trait HasINTdiv extends HasINT {
  self: Environment =>

  val intDiv = new HasEnvironment with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (_, INT(0)) => None
      case (INT(0), b) if b.isGround => Some(INT(0))
      case (INT(a), INT(b)) => Some(INT(a / b))
      case _ => None
    }

    override val name: String = "_/Int_"
  }
}
