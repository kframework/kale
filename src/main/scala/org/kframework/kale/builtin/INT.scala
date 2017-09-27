package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class INT(implicit protected val penv: Environment with BooleanMixin) {

  import penv._

  val Int = new ReferenceLabel[Int]("Int@INT-SYNTAX") {
    override protected[this] def internalInterpret(s: String): Int = s.toInt
  }

  val plus = PrimitiveFunction2[Int]("_+Int_", Int, _ + _)
  val minus = PrimitiveFunction2[Int]("_-Int_", Int, _ - _)
  val mult = PrimitiveFunction2[Int]("_*Int_", Int, _ * _)
  val div = PrimitiveFunction2[Int]("_/Int_", Int, _ / _)
  val mod = PrimitiveFunction2[Int]("_%Int_", Int, _ % _)
  val lt = PrimitiveFunction2[Int, Boolean]("_<Int_", Int, BOOLEAN.Boolean, _ < _)
  val le = PrimitiveFunction2[Int, Boolean]("_<=Int_", Int, BOOLEAN.Boolean, _ <= _)
  val gt = PrimitiveFunction2[Int, Boolean]("_>Int_", Int, BOOLEAN.Boolean, _ > _)
  val ge = PrimitiveFunction2[Int, Boolean]("_>=Int_", Int, BOOLEAN.Boolean, _ >= _)
  val neq = PrimitiveFunction2[Int, Boolean]("_=/=Int_", Int, BOOLEAN.Boolean, _ != _)
  val eq = PrimitiveFunction2[Int, Boolean]("_==Int_", Int, BOOLEAN.Boolean, _ == _)

  lazy val all = Set(Int, plus, minus, mult, div, mod, lt, le, gt, ge, neq, eq)
}

trait IntMixin extends kale.IntMixin {
  protected val env: Environment with BooleanMixin

  val INT = builtin.INT()(env)

  implicit val upInt = Up(INT.Int)
}
