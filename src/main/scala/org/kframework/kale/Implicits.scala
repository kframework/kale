package org.kframework.kale

import scala.language.implicitConversions

class Implicits(implicit env: Environment) extends StaticImplicits {
  import env.builtin._

  implicit def intConstant(x: Int): Constant[Int] = INT(x)
  implicit def doubleConstant(x: Double): Constant[Double] = DOUBLE(x)
  implicit def booleanConstant(x: Boolean): Constant[Boolean] = BOOLEAN(x)
  implicit def stringConstant(x: String): Constant[String] = STRING(x)

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = INT.+(x, y)
  }

  implicit class RichTerm(t: Term)(implicit env: Environment) {
    def :=(tt: Term)(implicit m: transformer.Binary.Apply): Term = m(t, tt)
  }
}

trait StaticImplicits {
  implicit class StaticRichTerm(t: Term) {
    def contains(subterm: Term): Boolean = if (t == subterm) true else t.exists(_.contains(subterm))
  }
}

object StaticImplicits extends StaticImplicits