package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.standard.StandardEnvironment
import org.scalatest.FreeSpec

class SubstitutionTest extends TestSetup[StandardEnvironment]() {

  import env._

  "substitution" in {
    val s = And.substitution(Map(X -> (5: Term)))
    val substitution = substitutionApplier(s)

    assert(substitution(5) === (5: Term))
    assert(substitution(X) === (5: Term))
    assert(substitution(Y) === Y)
    assert(substitution(Y + X) === Y + 5)
  }

  "context substitution" in {
    val s = And.substitution(Map(X -> buz(bar(1), X_1), Y -> bar(2)))
    val substitution = substitutionApplier(s)

    assert(substitution(
      foo(3, AnywhereContext(X, bar(Y)))
    ) === foo(3, buz(bar(1), bar(bar(2)))))
  }
}
