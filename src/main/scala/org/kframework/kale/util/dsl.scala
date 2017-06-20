package org.kframework.kale.util

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.builtin.importINT
import org.kframework.kale.standard.{SimpleFreeLabel2, Sort, StandardEnvironment}

import scala.language.implicitConversions

class dsl(implicit val env: StandardEnvironment) {

  import env._

  implicit class RichStandardTerm(t: Term) {
    def :=(tt: Term): Term = env.And.filterOutNext(env.unify(t, tt))

    def :==(tt: Term): Term = env.unify(t, tt)

    def =:=(tt: Term): Term = env.And.filterOutNext(env.unify(t, tt))
  }

  val plus = env.uniqueLabels.getOrElse("+", SimpleFreeLabel2("+")).asInstanceOf[Label2]

  implicit class asTerm(x: Term) {
    def +(y: Term): Term = plus(x, y)
  }

  implicit def symbolWithApp(s: Symbol)(env: Environment) = new {
    val label = env.label(s.name)

    def apply[T](value: T): Term = label.asInstanceOf[LeafLabel[T]](value)

    def apply(_1: Term): Term = label.asInstanceOf[Label1](_1)

    def apply(_1: Term, _2: Term): Term = label.asInstanceOf[Label2](_1, _2)
  }

  val W = PrettyWrapper

  def __ = Variable.freshVariable()

  def A(implicit env: StandardEnvironment) = env.Variable("A")

  def B(implicit env: StandardEnvironment) = env.Variable("B")
}


