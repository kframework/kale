package org.kframework.kale.builtin

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.standard.ReferenceLabel

trait IntMixin extends kale.IntMixin {
  _: Environment with kale.BooleanMixin =>

  override val INT = new INT {
    implicit val Int = define[Int]("Int@INT-SYNTAX")(_.toInt)

    import BOOLEAN.Boolean

    val plus = define("_+Int_", (_: Int) + (_: Int))
    val minus = define("_-Int_", (_: Int) - (_: Int))
    val mult = define("_*Int_", (_: Int) * (_: Int))
    val div = define("_/Int_", (_: Int) / (_: Int))
    val mod = define("_%Int_", (_: Int) % (_: Int))
    val lt = define("_<Int_", (_: Int) < (_: Int))
    val le = define("_<=Int_", (_: Int) <= (_: Int))
    val gt = define("_>Int_", (_: Int) > (_: Int))
    val ge = define("_>=Int_", (_: Int) >= (_: Int))
    val neq = define("_=/=Int_", (_: Int) != (_: Int))
    val eq = define("_==Int_", (_: Int) == (_: Int))

    lazy val all = Set(Int, plus, minus, mult, div, mod, lt, le, gt, ge, neq, eq)
  }

  implicit val upInt = INT.Int
}
