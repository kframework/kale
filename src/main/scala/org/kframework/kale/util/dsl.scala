package org.kframework.kale.util

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.builtin.importINT
import org.kframework.kale.standard.{SimpleFreeLabel2, StandardEnvironment}

import scala.language.implicitConversions

class dsl(implicit val env: StandardEnvironment) {

  implicit class RichStandardTerm(t: Term) {
    def :=(tt: Term): Term = env.And.withNext.filterOurNext(env.unify(t, tt))
    def :==(tt: Term): Term = env.unify(t, tt)
    def =:=(tt: Term): Term = env.And.withNext.filterOurNext(env.unify(t, tt))
  }

  val plus = SimpleFreeLabel2("+")

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = plus(x, y)
  }

  implicit def symbolWithApp(s: Symbol)(env: Environment) = new {
    val label = env.label(s.name)

    def apply[T](value: T): Term = label.asInstanceOf[LeafLabel[T]](value)

    def apply(_1: Term): Term = label.asInstanceOf[Label1](_1)

    def apply(_1: Term, _2: Term): Term = label.asInstanceOf[Label2](_1, _2)
  }
}
