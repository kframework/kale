package org.kframework.kale.tests

import org.kframework.kale.{Invoke, _}

trait TestSetup {

  implicit val env = new Environment
  import env._
  import env.builtin._
  val implicits = new Implicits()

  val X = Variable("X")
  val Y = Variable("Y")

  val emptyList = FreeLabel0("emptyList")
  val listLabel = new AssocWithIdListLabel("listLabel", emptyList())

  val foo = FreeLabel2("foo")
  val bar = FreeLabel1("bar")
  val buz = FreeLabel2("buz")
  val (a, b, c, d, e) = (STRING("a"), STRING("b"), STRING("c"), STRING("d"), STRING("e"))
  val matched = FreeLabel1("matched")
  val traversed = FreeLabel1("traversed")
  val andMatchingY = FreeLabel0("andMatchingY")

  val allLabels = Set(foo, STRING, INT, INT.+, matched, traversed,
    andMatchingY, Variable, AnywhereContext, bar, buz, emptyList, listLabel)

  val unifier = Matcher(env)

  val substitutionApplier = SubstitutionApply(env)

  val X_1 = AnywhereContext.hole(X)

  def toAssert(t: Term): String = t match {
    case Variable(name) => name
    case t: Node => t.toString
  }
}
