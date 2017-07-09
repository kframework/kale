package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.standard.StandardEnvironment
import org.scalatest.FreeSpec

class MatchSpec extends TestSetup[StandardEnvironment]() {

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

  "Regex" in {
    assert((STRING.Regex("a.*c".r) :== STRING.String("abbbc")) === Next(STRING.String("abbbc")))
  }

  "Top matches anything" in {
    assert((Top :== a) === Next(a))
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

    "zero-level" in {
      assert((foo(a, AnywhereContext(X, b)) := foo(a, b)) === Equality(X, X_1))
    }

    "a bit more" in {
      assert((foo(a, AnywhereContext(X, b)) :== foo(a, traversed(b))) === And(Equality(X, traversed(X_1)), Next(foo(a, traversed(b)))))
    }

    "with traversal" in {
      val term = foo(a, traversed(matched(andMatchingY())))
      assert(
        (foo(a, AnywhereContext(X, matched(Y))) :== term)
          ===
          And(And.substitution(Map(X -> traversed(X_1), Y -> andMatchingY())), Next(term)))
    }

    "example on the board" in {
      val term = foo(3, buz(bar(1), bar(bar(2))))
      assert(
        (foo(3, AnywhereContext(X, bar(Y))) :== term)
          === Or(
          And(And.substitution(Map(X -> buz(X_1, bar(bar(2))), Y -> (1: Term))), Next(term)),
          And(And.substitution(Map(X -> buz(bar(1), X_1), Y -> bar(2))), Next(term)),
          And(And.substitution(Map(X -> buz(bar(1), bar(X_1)), Y -> (2: Term))), Next(term))
        )
      )
    }

    "assoc inside one element" in {
      val term = bar(el ~~ 1 ~~ 2 ~~ bar(2) ~~ bar(bar(3)))
      assert(
        (bar(AnywhereContext(X, bar(Y))) :== term)
          ===
          Or(
            And(And.substitution(Map(X -> (el ~~ 1 ~~ 2 ~~ X_1 ~~ bar(bar(3))), Y -> (2: Term))), Next(term)),
            And(And.substitution(Map(X -> (el ~~ 1 ~~ 2 ~~ bar(2) ~~ X_1), Y -> bar(3))), Next(term)),
            And(And.substitution(Map(X -> (el ~~ 1 ~~ 2 ~~ bar(2) ~~ bar(X_1)), Y -> (3: Term))), Next(term)))
      )
    }
  }

  "of anywhere contexts" - {

    val XX = Variable("XX")
    val YY = Variable("YY")

    val Y1 = Variable("Y1")
    val X1 = Variable("X1")
    val Z = Variable("Z")
    val Z1 = Variable("Z1")
    val XX1 = Variable("XX1")
    val YY1 = Variable("YY1")
    val ZZ = Variable("ZZ")
    val ZZ1 = Variable("ZZ1")

    "1" in {
      assert((AnywhereContext(XX, YY)
        := AnywhereContext(XX1, bar(YY1))
        ) !== Bottom
      )
    }

    "2" in {
      assert((AnywhereContext(XX, AnywhereContext(X, bar(YY)))
        := AnywhereContext(XX1, AnywhereContext(X1, bar(YY1)))
        ) !== Bottom
      )
    }

    "3" in {
      assert((AnywhereContext(XX, AnywhereContext(X, el ~~ YY ~~ a))
        := AnywhereContext(XX1, AnywhereContext(X1, el ~~ YY1 ~~ a))
        ) !== Bottom
      )
    }

    // TODO: ignored as it was slow
    "4" ignore {

      assert((foo(AnywhereContext(XX, AnywhereContext(X, Y)), AnywhereContext(YY, AnywhereContext(Z, ZZ)))
        := foo(AnywhereContext(XX1, AnywhereContext(X1, bar(Y1))), AnywhereContext(YY1, AnywhereContext(Z1, ZZ1))))
        !== Bottom)
    }
  }

  "of pattern contexts" - {

    val XX = Variable("XX")
    val YY = Variable("YY")

    "zero level" in {
      assert((CAPP(C, X) :== 1)
        === And(And.substitution(Map(C -> Hole, X -> 1)), Next(1)))
    }

    "one level" in {
      val term = foo(1, bar(2))
      assert((CAPP(C, bar(X)) :== term)
        === And(
        And.substitution(Map(C -> foo(1, Hole), X -> 2)),
        Next(term)))

    }

    "two levels" in {
      val term = foo(1, bar(bar(2)))
      assert((CAPP(C, bar(X)) :== term)
        === Or(
        And(And.substitution(Map(C -> foo(1, Hole), X -> bar(2))),
          Next(term)),
        And(And.substitution(Map(C -> foo(1, bar(Hole)), X -> 2)),
          Next(term))))
    }

    "stops traversal when encountering unknown" in {
      val term = foo(1, bar(buz(3, bar(2))))
      assert((CAPP(C, bar(X)) :== term)
        === And(And.substitution(Map(C -> foo(1, Hole), X -> buz(3, bar(2)))), Next(term)))
    }
  }

  "of multiple contexts" - {
    "pattern context in anywhere context" in {
      val ACx = Variable("ACx")
      val ACx_1 = Variable("ACx☐1")
      val term = buz(1, foo(2, bar(3)))

      assert((AnywhereContext(ACx, CAPP(C, bar(X))) :== term)
        === Or(
        And(And.substitution(Map(C -> Hole, X -> 3, ACx -> buz(1, foo(2, ACx_1)))), Next(term)),
        And(And.substitution(Map(C -> foo(2, Hole), X -> 3, ACx -> buz(1, ACx_1))), Next(term)))
      )
    }
  }

  "function defined by rewriting" - {
    "simple still stuck increment function" in {
      assert(a2b(c).label === a2b)
    }

    "simple applied increment function" ignore {
      assert(a2b(a) === b)
    }
  }

  "or" in {
    assert((Or(a, b) := a) === Top)
    assert((a := Or(a, b)) === Top)

    assert((bar(Or(a, b)) := bar(a))
      === Top)

    assert((bar(Or(a, b)) := bar(c))
      === Bottom)

    assert((bar(Or(X, Y)) := bar(c))
      === Or(Equality(X, c), Equality(Y, c)))

    assert((bar(Or(X, Y)) := Or(bar(a), bar(b)))
      === Or(Equality(X, a), Equality(Y, a), Equality(X, b), Equality(Y, b)))

    assert((bar(Or(X, Y)) := bar(And(a, Equality(Y, b))))
      === And(Equality(X, a), Equality(Y, b)))
  }

  // TODO: Daejun, not sure exactly what you're testing for here but it fails because there are multiple constraint terms
  "and" ignore {
    assert((X := And(a, Equality(X, a))) === Equality(X, a))
    assert((X := And(a, Equality(X, b))) === Bottom)

    val x = And.substitutionAndTerms(Equality.binding(X, a), Seq(b))
    val y = And.substitutionAndTerms(Equality.binding(Y, c), Seq(d))
    val xy = And.apply(x, y)
    assert(xy === And(Equality(X, a), Equality(Y, c), b, d))
    assert(xy === And(Equality(X, a), Equality(Y, c), d, b))

    assert(And.apply(Or(a, b), Or(c, d)) == Or(And(a, c), And(a, d), And(b, c), And(b, d)))

    // testing equivalence of DNFAndLabel.apply(Term,Term) vs DNFAndLabel.apply(Iterable[Term])
    assert(And.apply(And(a, b), c) == And.apply(Seq(And(a, b), c)))
  }

  "context and and" in {
    assert((And(CAPP(C, bar(X)), Equality(X, 2)) := foo(1, bar(2)))
      === And.substitution(Map(C -> foo(1, Hole), X -> 2)))

    assert((And(CAPP(C, bar(X)), Equality(X, 3)) := foo(1, bar(2)))
      === Bottom)
  }

  "mix contexts and logical operators" in {
    assert((buz(And(CAPP(C, bar(X)), Equality(X, 2)), bar(X)) := buz(foo(1, bar(2)), bar(2)))
      === And.substitution(Map(C -> foo(1, Hole), X -> 2)))

    assert((buz(And(CAPP(C, bar(X)), Equality(X, 2)), And(bar(X), Equality(Y, 2))) := buz(foo(1, bar(2)), bar(2)))
      === And.substitution(Map(C -> foo(1, Hole), X -> 2, Y -> 2)))

    assert((buz(And(CAPP(C, bar(X)), Equality(X, 2), Equality(Y, 2)), And(bar(X), Equality(Y, 2))) := buz(foo(1, bar(2)), bar(2)))
      === And.substitution(Map(C -> foo(1, Hole), X -> 2, Y -> 2)))

    assert((buz(And(CAPP(C, bar(X)), Equality(X, 2), Equality(Y, 3)), And(bar(X), Equality(Y, 2))) := buz(foo(1, bar(2)), bar(2)))
      === Bottom)

    assert((buz(And(CAPP(C, bar(X)), Equality(X, 2)), And(bar(X), Equality(Y, 2))) := buz(foo(1, bar(2)), Or(bar(2), bar(3))))
      === And.substitution(Map(C -> foo(1, Hole), X -> 2, Y -> 2)))

    assert((buz(And(CAPP(C, bar(X)), Equality(X, 2)), And(bar(Y), Equality(Y, 3))) := buz(foo(1, bar(2)), Or(bar(2), bar(3))))
      === And.substitution(Map(C -> foo(1, Hole), X -> 2, Y -> 3)))
  }

  "not" - {
    "pass" in {
      assert((X := And(a, Not(Equality(X, b)))) === Equality(X, a))
    }
    "to bottom" in {
      assert((X := And(a, Not(Equality(X, a)))) === Bottom)
    }

    "leave condition in place" in {
      assert((X :== Or(a, And(Y, Not(Equality(X, a))))) ===
        Or(And(Equality(X, a), Next(a)),
          And(Equality(X, Y), Not(Equality(X, a)), Next(Y))))
    }
  }

  "bind match" in {
    assert((BindMatch(X, bar(Y)) := bar(3)) === And(Equality(X, bar(3)), Equality(Y, 3)))
    assert((BindMatch(X, bar(Y)) := 3) === Bottom)

    assert((BindMatch(X, bar(Y)) := Or(bar(1), 2)) === And(Equality(X, bar(1)), Equality(Y, 1)))
  }

  "ForAll" in {
    assert((ForAll(X, X) :== X) === Next(X))
    assert((ForAll(X, X) :== 3) === Next(3))
    assert((ForAll(X, bar(X)) :== 3) === Bottom)
    assert((ForAll(X, bar(X)) :== bar(3)) === Next(bar(3)))
    assert((ForAll(X, foo(X, Y)) :== foo(3, 4)) === And(Equality(Y, 4), Next(foo(3, 4))))
  }
}
