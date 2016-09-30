package org.kframework.kale.tests

import org.kframework.kale._
import context._
import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec with TestSetup {

  import env._
  import implicits._

  "simple" in {
    assert(unifier(X, 5) === Equality(X, 5))
    assert(unifier(X + Y, (5: Term) + 7) === And.createSubstitution(Map(X -> (5: Term), Y -> (7: Term))))
    assert(unifier(X + X, (5: Term) + 7) === Bottom)
    assert(unifier((5: Term) + 7, (5: Term) + 7) === Top)
    //    assert((2: Term).unify(5: Term) == Bottom)
    //    assert((2: Term).unify(2: Term) == Top)
  }

  "assoc" in {
    assert(unifier(listLabel(X, 5), listLabel(3, 5)) === Equality(X, 3))
    assert(unifier(listLabel(3, 4, X, 7), listLabel(3, 4, 5, 6, 7)) === Equality(X, listLabel(5, 6)))
    assert(unifier(listLabel(3, X, 5, Y, 7), listLabel(3, 4, 5, 6, 7)) === And.createSubstitution(Map(X -> (4: Term), Y -> (6: Term))))
    assert(unifier(listLabel(X, 5, Y), listLabel(3, 4, 5, 6, 7)) === And.createSubstitution(Map(X -> listLabel(3, 4), Y -> listLabel(6, 7))))
    val res = unifier(listLabel(3, X, Y, 6), listLabel(3, 4, 5, 6))
    assert(unifier(listLabel(3, X, Y, 6), listLabel(3, 4, 5, 6)) ===
      Or(
        And.createSubstitution(Map(X -> emptyList(), Y -> listLabel(4, 5))),
        And.createSubstitution(Map(X -> (4: Term), Y -> (5: Term))),
        And.createSubstitution(Map(X -> listLabel(4, 5), Y -> emptyList())))
    )
  }

  "contexts" - {

    implicit val m = Matcher(env)

    "zero-level" in {
      assert((foo(a, AnywhereContext(X, b)) := foo(a, b)) === Equality(X, X_1))
    }

    "a bit more" in {
      assert((foo(a, AnywhereContext(X, b)) := foo(a, traversed(b))) === Equality(X, traversed(X_1)))
    }

    "with traversal" in {
      assert(
        (foo(a, AnywhereContext(X, matched(Y))) := foo(a, traversed(matched(andMatchingY()))))
          ===
          And.createSubstitution(Map(X -> traversed(X_1), Y -> andMatchingY())))
    }

    "example on the board" in {
      assert(
        (foo(3, AnywhereContext(X, bar(Y))) := foo(3, buz(bar(1), bar(bar(2)))))
          === Or(
          And.createSubstitution(Map(X -> buz(X_1, bar(bar(2))), Y -> (1: Term))),
          And.createSubstitution(Map(X -> buz(bar(1), X_1), Y -> bar(2))),
          And.createSubstitution(Map(X -> buz(bar(1), bar(X_1)), Y -> (2: Term)))
        )
      )
    }

    "assoc inside one element" in {
      assert(
        (bar(AnywhereContext(X, bar(Y))) := bar(listLabel(1, 2, bar(2), bar(bar(3)))))
          ===
          Or(
            And.createSubstitution(Map(X -> listLabel(1, 2, X_1, bar(bar(3))), Y -> (2: Term))),
            And.createSubstitution(Map(X -> listLabel(1, 2, bar(2), X_1), Y -> bar(3))),
            And.createSubstitution(Map(X -> listLabel(1, 2, bar(2), bar(X_1)), Y -> (3: Term)))
          )
      )
    }
  }

  "of contexts" - {
    implicit val m = Matcher(env)

    val XX = Variable("XX")
    val YY = Variable("YY")

    "identical simple" in {
      assert(AnywhereContext(XX, AnywhereContext(X, listLabel(YY, a)))
        := AnywhereContext(XX, AnywhereContext(X, listLabel(YY, a))) !== Bottom)
    }

    "identical" in {
      val Y1 = Variable("Y1")

      assert(foo(AnywhereContext(XX, AnywhereContext(X, Y)), AnywhereContext(XX, AnywhereContext(X, YY)))
        := foo(AnywhereContext(XX, AnywhereContext(X, bar(Y1))), AnywhereContext(XX, AnywhereContext(X, YY))) !== Bottom)
    }
  }
}
