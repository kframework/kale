package org.kframework.kale.tests

import org.kframework.kale.{Matcher, _}

trait TestSetup {

  val X = Variable("X")
  val Y = Variable("Y")

  val emptyList = FreeLabel0(UniqueId(), "emptyList")
  val listLabel = AssocWithIdListLabel("listLabel", emptyList())

  val foo = FreeLabel2("foo")
  val bar = FreeLabel1("bar")
  val buz = FreeLabel2("buz")
  val (a, b, c, d) = (STRING("a"), STRING("b"), STRING("c"), STRING("d"))
  val matched = FreeLabel1("matched")
  val traversed = FreeLabel1("traversed")
  val andMatchingY = FreeLabel0("andMatchingY")

  val allLabels = Set(foo, STRING, INT, INT.+, matched, traversed,
    andMatchingY, Variable, AnywhereContext, bar, buz, emptyList, listLabel)

  val unifier = Matcher(allLabels)

  val substitutionApplier = SubstitutionApply(allLabels)

  val X_1 = AnywhereContext.hole(X)

  def toAssert(t: Term): String = t match {
    case Variable(name) => name
    case t: Node => t.toString
  }
}
