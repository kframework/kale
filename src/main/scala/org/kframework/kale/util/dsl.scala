package org.kframework.kale.util

import org.kframework.kale._
import org.kframework.kale.standard.StandardEnvironment

import scala.language.implicitConversions

trait DSLMixin {
  self: StandardEnvironment =>
  val DSL = new dsl()
}

class dsl(implicit val env: StandardEnvironment) {

  import env._

  implicit class RichStandardTerm(t: Term) {
    def :=(tt: Term): Term = env.And.filterOutNext(env.unify(t, tt))

    def :==(tt: Term): Term = env.unify(t, tt)

    def ==>(tt: Term): Term = Rewrite(t, tt)

    def ?=>(tt: Term): Term = STRATEGY.orElseLeave(Rewrite(t, tt))

    def =:=(tt: Term): Term = env.And.onlyNext(env.unify(t, tt))
  }

  implicit def symbolWithApp(s: Symbol)(env: Environment) = new {
    val label = env.label(s.name)

    def apply[T](value: T): Term = label.asInstanceOf[LeafLabel[T]](value)

    def apply(_1: Term): Term = label.asInstanceOf[Label1](_1)

    def apply(_1: Term, _2: Term): Term = label.asInstanceOf[Label2](_1, _2)
  }

  def __ = Variable.freshVariable()

  def A(implicit env: StandardEnvironment) = env.Variable("A")

  def B(implicit env: StandardEnvironment) = env.Variable("B")

  val Condition = Variable("Condition")
  val Then = Variable("Then")
  val Else = Variable("Else")
  val X = Variable("X")
  val Y = Variable("Y")
  val P = Variable("P")
  val S = Variable("S")
}
