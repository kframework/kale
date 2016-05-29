package org.kframework.kale.tests

import org.kframework.kale.{AnywhereContext, SimpleMatcher, _}
import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec with TestSetup {

  import Implicits._

  "simple" in {
    assert(unifier(X, 5) === Equality(X, 5))
    assert(unifier(X + Y, (5: Term) + 7) === Substitution(Map(X -> (5: Term), Y -> (7: Term))))
    assert(unifier(X + X, (5: Term) + 7) === Bottom)
    assert(unifier((5: Term) + 7, (5: Term) + 7) === Top)
    //    assert((2: Term).unify(5: Term) == Bottom)
    //    assert((2: Term).unify(2: Term) == Top)
  }

  "assoc" in {
    assert(unifier(listLabel(X, 5), listLabel(3, 5)) === Equality(X, 3))
    assert(unifier(listLabel(3, 4, X, 7), listLabel(3, 4, 5, 6, 7)) === Equality(X, listLabel(5, 6)))
    assert(unifier(listLabel(3, X, 5, Y, 7), listLabel(3, 4, 5, 6, 7)) === Substitution(Map(X -> (4: Term), Y -> (6: Term))))
    assert(unifier(listLabel(X, 5, Y), listLabel(3, 4, 5, 6, 7)) === Substitution(Map(X -> listLabel(3, 4), Y -> listLabel(6, 7))))
    val res = unifier(listLabel(3, X, Y, 6), listLabel(3, 4, 5, 6))
    assert(unifier(listLabel(3, X, Y, 6), listLabel(3, 4, 5, 6)) ===
      Or(
        Substitution(Map(X -> emptyList(), Y -> listLabel(4, 5))),
        Substitution(Map(X -> (4: Term), Y -> (5: Term))),
        Substitution(Map(X -> listLabel(4, 5), Y -> emptyList())))
    )
  }

  "contexts" - {
    val foo = FreeLabel2("foo")
    val bar = FreeLabel1("bar")
    val buz = FreeLabel2("buz")
    val (a, b, c, d) = (STRING("a"), STRING("b"), STRING("c"), STRING("d"))
    val matched = FreeLabel1("matched")
    val traversed = FreeLabel1("traversed")
    val andMatchingY = FreeLabel0("andMatchingY")

    val contextsLabels = Set(foo, STRING, INT, matched, traversed,
      andMatchingY, Variable, AnywhereContext, bar, buz, emptyList, listLabel)

    implicit val m = SimpleMatcher(contextsLabels)

    val X_1 = AnywhereContext.hole(X)

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
          Substitution(Map(X -> traversed(X_1), Y -> andMatchingY())))
    }

    "example on the board" in {
      assert(
        (foo(3, AnywhereContext(X, bar(Y))) := foo(3, buz(bar(1), bar(bar(2)))))
          === Or(
          Substitution(Map(X -> buz(X_1, bar(bar(2))), Y -> (1: Term))),
          Substitution(Map(X -> buz(bar(1), X_1), Y -> bar(2))),
          Substitution(Map(X -> buz(bar(1), bar(X_1)), Y -> (2: Term)))
        )
      )
    }

    "assoc inside one element" in {
      assert(
        (bar(AnywhereContext(X, bar(Y))) := bar(listLabel(1, 2, bar(2), bar(bar(3)))))
          ===
          Or(
            Substitution(Map(X -> listLabel(1, 2, X_1, bar(bar(3))), Y -> (2: Term))),
            Substitution(Map(X -> listLabel(1, 2, bar(2), X_1), Y -> bar(3))),
            Substitution(Map(X -> listLabel(1, 2, bar(2), bar(X_1)), Y -> (3: Term)))
          )
      )
    }
  }
}
