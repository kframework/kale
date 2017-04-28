package org.kframework.kale.builtin

import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.{Environment, FunctionLabel2, Term}

trait HasDOUBLE {
  self: Environment =>

  val DOUBLE = new ReferenceLabel[Double]("Double")(this) {
    override protected[this] def internalInterpret(s: String): Double = s.toDouble
  }
}

trait HasDOUBLEdiv {
  self: Environment with HasDOUBLE =>

  val DOUBLEdiv = new HasEnvironment with FunctionLabel2 {
    def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
      case (_, DOUBLE(0)) => None
      case (DOUBLE(0), b) if b.isGround => Some(DOUBLE(0))
      case (DOUBLE(a), DOUBLE(b)) => Some(DOUBLE(a / b))
      case _ => None
    }

    override val name: String = "_/Double_"
  }
}
