package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

case class INT(implicit protected val penv: Environment with importBOOLEAN) extends Module("INT") {

  import penv._

  val Int = new ReferenceLabel[Int]("Int") {
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

  lazy val all = Set(Int, plus, minus, mult, div, mod, lt, le, gt, ge)
}

trait importINT {
  protected val env: Environment with importBOOLEAN
  val BOOLEAN: builtin.BOOLEAN

  val INT = builtin.INT()(env)

  implicit def toINT(i: Int): DomainValue[Int] = INT.Int(i)
}
