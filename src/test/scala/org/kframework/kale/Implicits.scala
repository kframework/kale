package org.kframework.kale

import scala.language.implicitConversions

object Implicits {

  implicit def intConstant(x: Int): Constant[Int] = INT(x)

  implicit class asTerm(x: Term) {
    def +(y: Term): Node2 = INT.+(x, y)
  }
}
