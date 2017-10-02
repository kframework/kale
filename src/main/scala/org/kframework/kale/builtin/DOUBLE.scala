package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale.standard.ReferenceLabel
import org.kframework.kale.util.Named
import org.kframework.kale.{FunctionLabel2, _}

trait DoubleMixin extends kale.DoubleMixin {
  _: Environment =>

  override val DOUBLE = new {
    val Double = new ReferenceLabel[Double]("Double") {
      override protected[this] def internalInterpret(s: String): Double = s.toDouble
    }

    val div = new Named("_/Double_") with FunctionLabel2 {
      override val isPredicate: Option[Boolean] = Some(false)

      def f(_1: Term, _2: Term): Option[Term] = (_1, _2) match {
        case (_, Double(0)) => None
        case (Double(0), b) if b.isGround => Some(Double(0))
        case (Double(a), Double(b)) => Some(Double(a / b))
        case _ => None
      }
    }
  }

  implicit val upDouble = DOUBLE.Double

}
