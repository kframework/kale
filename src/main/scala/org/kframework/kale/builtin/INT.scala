package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

trait IntMixin extends kale.IntMixin {
  _: Environment with kale.BooleanMixin =>

  override val INT = new INT {
    implicit val Int = new ReferenceLabel[scala.Int]("Int@INT-SYNTAX") {
      override protected[this] def internalInterpret(s: String): Int = s.toInt
    }

    val plus = PrimitiveFunction2[scala.Int]("_+Int_", Int, _ + _)
    val minus = PrimitiveFunction2[scala.Int]("_-Int_", Int, _ - _)
    val mult = PrimitiveFunction2[scala.Int]("_*Int_", Int, _ * _)
    val div = PrimitiveFunction2[scala.Int]("_/Int_", Int, _ / _)
    val mod = PrimitiveFunction2[scala.Int]("_%Int_", Int, _ % _)
    val lt = PrimitiveFunction2[scala.Int, scala.Boolean]("_<Int_", Int, BOOLEAN.Boolean, _ < _)
    val le = PrimitiveFunction2[scala.Int, scala.Boolean]("_<=Int_", Int, BOOLEAN.Boolean, _ <= _)
    val gt = PrimitiveFunction2[scala.Int, scala.Boolean]("_>Int_", Int, BOOLEAN.Boolean, _ > _)
    val ge = PrimitiveFunction2[scala.Int, scala.Boolean]("_>=Int_", Int, BOOLEAN.Boolean, _ >= _)
    val neq = PrimitiveFunction2[scala.Int, scala.Boolean]("_=/=Int_", Int, BOOLEAN.Boolean, _ != _)
    val eq = PrimitiveFunction2[scala.Int, Boolean]("_==Int_", Int, BOOLEAN.Boolean, _ == _)

    lazy val all = Set(Int, plus, minus, mult, div, mod, lt, le, gt, ge, neq, eq)
  }

  implicit val upInt = INT.Int
}
