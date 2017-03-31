package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.context._
import org.scalactic.Prettifier
import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec with TestSetup {

  import env._
  import implicits._

  "simple" in {
    assert(unifier(X, 5) === Equality(X, 5))
    assert(unifier(X + Y, (5: Term) + 7) === And.substitution(Map(X -> (5: Term), Y -> (7: Term))))
    assert(unifier(X + X, (5: Term) + 7) === Bottom)
    assert(unifier((5: Term) + 7, (5: Term) + 7) === Top)
    //    assert((2: Term).unify(5: Term) == Bottom)
    //    assert((2: Term).unify(2: Term) == Top)
  }

  "assoc" in {
    assert(unifier(X ~~ 5, el ~~ 3 ~~ 5) === Equality(X, 3))
    assert(unifier(el ~~ 3 ~~ 4 ~~ X ~~ 7, el ~~ 3 ~~ 4 ~~ 5 ~~ 6 ~~ 7) === Equality(X, el ~~ 5 ~~ 6))
    assert(unifier(el ~~ 3 ~~ X ~~ 5 ~~ Y ~~ 7, el ~~ 3 ~~ 4 ~~ 5 ~~ 6 ~~ 7) === And.substitution(Map(X -> (4: Term), Y -> (6: Term))))
    assert(unifier(el ~~ X ~~ 5 ~~ Y, el ~~ 3 ~~ 4 ~~ 5 ~~ 6 ~~ 7) === And.substitution(Map(X -> (el ~~ 3 ~~ 4), Y -> (el ~~ 6 ~~ 7))))
    val res = unifier(el ~~ 3 ~~ X ~~ Y ~~ 6, el ~~ 3 ~~ 4 ~~ 5 ~~ 6)
    assert(unifier(el ~~ 3 ~~ X ~~ Y ~~ 6, el ~~ 3 ~~ 4 ~~ 5 ~~ 6) ===
      Or(
        And.substitution(Map(X -> el, Y -> el ~~ 4 ~~ 5)),
        And.substitution(Map(X -> (4: Term), Y -> (5: Term))),
        And.substitution(Map(X -> (el ~~ 4 ~~ 5), Y -> el))
      ))
  }

  "contexts" - {

    implicit val m = new Matcher(env)

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
          And.substitution(Map(X -> traversed(X_1), Y -> andMatchingY())))
    }

    "example on the board" in {
      assert(
        (foo(3, AnywhereContext(X, bar(Y))) := foo(3, buz(bar(1), bar(bar(2)))))
          === Or(
          And.substitution(Map(X -> buz(X_1, bar(bar(2))), Y -> (1: Term))),
          And.substitution(Map(X -> buz(bar(1), X_1), Y -> bar(2))),
          And.substitution(Map(X -> buz(bar(1), bar(X_1)), Y -> (2: Term)))
        )
      )
    }

    "assoc inside one element" in {
      assert(
        (bar(AnywhereContext(X, bar(Y))) := bar(el ~~ 1 ~~ 2 ~~ bar(2) ~~ bar(bar(3))))
          ===
          Or(
            And.substitution(Map(X -> (el ~~ 1 ~~ 2 ~~ X_1 ~~ bar(bar(3))), Y -> (2: Term))),
            And.substitution(Map(X -> (el ~~ 1 ~~ 2 ~~ bar(2) ~~ X_1), Y -> bar(3))),
            And.substitution(Map(X -> (el ~~ 1 ~~ 2 ~~ bar(2) ~~ bar(X_1)), Y -> (3: Term))))
      )
    }
  }

  "of anywhere contexts" - {
    implicit val m = new Matcher(env)

    val XX = Variable("XX")
    val YY = Variable("YY")

    "identical simple" in {
      assert((AnywhereContext(XX, AnywhereContext(X, el ~~ YY ~~ a))
        := AnywhereContext(XX, AnywhereContext(X, el ~~ YY ~~ a))
        ) !== Bottom
      )
    }

    "identical" in {
      val Y1 = Variable("Y1")

      assert(foo(AnywhereContext(XX, AnywhereContext(X, Y)), AnywhereContext(XX, AnywhereContext(X, YY)))
        := foo(AnywhereContext(XX, AnywhereContext(X, bar(Y1))), AnywhereContext(XX, AnywhereContext(X, YY))) !== Bottom)
    }
  }

  "of pattern contexts" - {
    implicit val m = new Matcher(env)

    val XX = Variable("XX")
    val YY = Variable("YY")

    "zero level" in {
      assert((CAPP(C, X) := 1)
        === And.substitution(Map(C -> Hole, X -> 1)))
    }

    "one level" in {
      assert((CAPP(C, bar(X)) := foo(1, bar(2)))
        === And.substitution(Map(C -> foo(1, Hole), X -> 2)))
    }

    "two levels" in {
      assert((CAPP(C, bar(X)) := foo(1, bar(bar(2))))
        === Or(And.substitution(Map(C -> foo(1, Hole), X -> bar(2))),
        And.substitution(Map(C -> foo(1, bar(Hole)), X -> 2))))
    }

    "stops traversal when encountering unknown" in {
      assert((CAPP(C, bar(X)) := foo(1, bar(buz(3, bar(2)))))
        === And.substitution(Map(C -> foo(1, Hole), X -> buz(3, bar(2)))))
    }
  }

  "function defined by rewriting" - {
    "simple still stuck increment function" in {
      assert(a2b(c).label === a2b)
    }

    "simple applied increment function" in {
      assert(a2b(a) === b)
    }
  }
}
