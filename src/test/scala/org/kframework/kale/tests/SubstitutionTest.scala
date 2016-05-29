package org.kframework.kale.tests

import org.kframework.kale.{Implicits, Substitution, Term}
import org.scalatest.FreeSpec

class SubstitutionTest extends FreeSpec with TestSetup {

  import Implicits._

  "substitution" in {
    val s = Substitution(Map(X -> (5: Term)))
    val substitution = substitutionApplier(s)

    assert(substitution(5) === (5: Term))
    assert(substitution(X) === (5: Term))
    assert(substitution(Y) === Y)
    assert(substitution(Y + X) === Y + 5)
  }
}
