package org.kframework.kale

import scala.language.implicitConversions

object Implicits {

  implicit def intConstant(x: Int): Constant[Int] = INT(x)

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = INT.+(x, y)
  }

  implicit class RichTerm(t: Term)(implicit m: Dispatch) {
    def :=(tt: Term) = m(t, tt)
  }
}

