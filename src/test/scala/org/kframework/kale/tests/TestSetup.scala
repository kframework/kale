package org.kframework.kale.tests

import org.kframework.kale.{ApplySubstitution, SimpleMatcher, _}

trait TestSetup {

  val X = Variable("X")
  val Y = Variable("Y")

  val emptyList = FreeLabel0(UniqueId(), ".List")
  val listLabel = AssocWithIdListLabel("_,_", emptyList())

  val allLabels = Set(Variable, INT.+, INT, emptyList, listLabel)

  val unifier = SimpleMatcher(allLabels)

  val substitutionApplier = ApplySubstitution(allLabels)
}
