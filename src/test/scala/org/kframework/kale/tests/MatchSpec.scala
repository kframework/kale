package org.kframework.kale.tests

import org.kframework.kale._
import org.scalatest.FreeSpec
import collection._

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

  "of multiple contexts" - {
    "pattern context in anywhere context" in {
      val ACx = Variable("ACx")
      val ACx_1 = Variable("ACx_1")
      val res = (AnywhereContext(ACx, CAPP(C, bar(X))) := buz(1, foo(2, bar(3))))

      assert((AnywhereContext(ACx, CAPP(C, bar(X))) := buz(1, foo(2, bar(3))))
        === Or(
        And.substitution(Map(C -> Hole, X -> 3, ACx -> buz(1, foo(2, ACx_1)))),
        And.substitution(Map(C -> foo(2, Hole), X -> 3, ACx -> buz(1, ACx_1)))
      ))
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

  "and" in {
    assert((X := And(a, Equality(X, a))) === Equality(X, a))
    assert((X := And(a, Equality(X, b))) === Bottom)

    val x = And.substitutionAndTerms.apply(Equality.binding(X, a), Seq(b))
    val y = And.substitutionAndTerms.apply(Equality.binding(Y, c), Seq(d))
    val xy = And.apply(x, y)
    assert(xy == And(Equality(X, a), Equality(Y, c), b, d))
    assert(xy == And(Equality(X, a), Equality(Y, c), d, b))

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

  "not" in {
    assert((X := And(a, Not(Equality(X, b)))) === Equality(X, a))
    assert((X := And(a, Not(Equality(X, a)))) === Bottom)

    assert((X := Or(a, Not(Equality(X, a)))) === Or(Equality(X, a), And(X, Not(Equality(X, a)))))
  }

  "if then else" in {
    assert((IfThenElse(a, Equality(X, a), Equality(X, b)) := a) === Equality(X, a))
    assert((IfThenElse(a, Equality(X, a), Equality(X, b)) := b) === Equality(X, b))
    assert((foo(X, IfThenElse(Equality(X, a), a, b)) := foo(a, a)) === Equality(X, a))
    assert((foo(X, IfThenElse(Equality(X, a), a, b)) := foo(b, b)) === Equality(X, b))
    assert((foo(X, IfThenElse(Equality(X, a), a, b)) := foo(a, b)) === Bottom)
    assert((foo(X, IfThenElse(Equality(X, a), a, b)) := foo(b, a)) === Bottom)
  }

  "bind match" in {
    assert((BindMatch(X, bar(Y)) := bar(3)) === And(Equality(X, bar(3)), Equality(Y, 3)))
    assert((BindMatch(X, bar(Y)) := 3) === Bottom)

    assert((BindMatch(X, bar(Y)) := Or(bar(1), 2)) === And(Equality(X, bar(1)), Equality(Y, 1)))
  }

  "pretty" - {
    val three = PrettyWrapper("a", 3, "b")
    "ground" in {

      assert(three.toString === "a3b")

      assert(((3: Term) := three) === Top)

      assert((X := three) === Equality(X, three))
    }

    val fooThree = PrettyWrapper("c", foo(three, 6), "d")

    "wrapper left" in {
      assert(And(foo(X, 6) := fooThree, Equality(X, 3)) === Equality(X, three))
    }

    "wrapper right" in {
      assert(And(fooThree := foo(X, 6), Equality(X, 3)) === Equality(X, three))
    }

    "wrapper wrapper" in {
      val fooThreeDifferent = PrettyWrapper("e", foo(three, 6), "f")
      assert((fooThree := fooThree) == Top)
      assert((fooThree := fooThreeDifferent) == Bottom)
    }
  }
}
