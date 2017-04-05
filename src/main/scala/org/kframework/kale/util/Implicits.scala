package org.kframework.kale.util

import org.kframework.kale._
import org.kframework.kale.standard.{CurrentEnvironment, FreeLabel2}

import scala.language.implicitConversions

class Implicits(implicit env: CurrentEnvironment) extends StaticImplicits {

  import env.builtin._

  implicit def intConstant(x: Int): Constant[Int] = INT(x)
  implicit def doubleConstant(x: Double): Constant[Double] = DOUBLE(x)
  implicit def booleanConstant(x: Boolean): Constant[Boolean] = BOOLEAN(x)
  implicit def stringConstant(x: String): Constant[String] = STRING(x)

  val plus = FreeLabel2("+")//PrimitiveFunction2("+", INT, (a: Int, b: Int) => a + b)(env)

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = plus(x, y)
  }

  implicit class RichTerm(t: Term)(implicit env: Environment) {
    def :=(tt: Term)(implicit m: Matcher): Term = m(t, tt)
  }

}

trait StaticImplicits {

  implicit class StaticRichTerm(t: Term) {
    def contains(subterm: Term): Boolean = if (t == subterm) true else t.exists(_.contains(subterm))
  }

  implicit class StaticRichAssocLabel(label: AssocLabel) {
    def apply(args: Term*): Term = label.apply(args)
  }
}

object StaticImplicits extends StaticImplicits