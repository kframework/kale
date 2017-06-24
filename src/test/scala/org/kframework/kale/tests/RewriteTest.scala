package org.kframework.kale.tests

import org.kframework.kale._
import org.scalatest.FreeSpec

import scala.collection._
import scala.language.implicitConversions

class RewriteTest extends FreeSpec with TestSetup {

  import env._
  import implicits._

  def assertRewrite(rule: Rewrite)(obj: Term, expected: Term) {
    val actual = unify(rule, obj)
    //    val unificationRes = unifier(rule._1, obj)
    //    val res = Or.asSet(unificationRes) map (s => substitutionApplier(s.asInstanceOf[Substitution])(rule._2))
    assert(justNext(actual) === expected)
  }

  def assertRewrite(rule0: Term)(obj: Term, expected: Term) {
    val rule = moveRewriteSymbolToTop(rule0)(env)
    assertRewrite(rule)(obj, expected)
  }

  "X + 0 => X" in {
    assertRewrite(Rewrite(X + 0, X))((5: Term) + 0, 5: Term)
  }

  "2 + X + 3 => 5 + X" in {
    assertRewrite(Rewrite((2: Term) + X + 3, (5: Term) + X))((2: Term) + 4 + 3, (5: Term) + 4)
  }

  val rewriter = Rewriter(substitutionApplier, matcher)(Set(
    Rewrite(X + 0, X),
    Rewrite((0: Term) + X, X),
    Rewrite(el ~~ 3 ~~ X ~~ Y ~~ 6, el ~~ X ~~ 0 ~~ Y)
  ))

  def justNext(t: Term) = t.asOr map {
    case And.withNext(_, Some(Next(next))) => next
  }

  "inner rewrite" - {
    "simple" in {
      assert(justNext(bar(Rewrite(X, b)) :== bar(a)) === bar(b))
    }
    "swap" in {
      assert(justNext(foo(Rewrite(X, Y), Rewrite(Y, X)) :== foo(a, b)) === foo(b, a))
    }
  }

  "step" in {
    assert(rewriter.step((1: Term) + 0).toList === List(1: Term))
    assert(rewriter.step(1: Term).toList === List())
  }

  "search" in {
    assert(rewriter.searchStep((1: Term) + 0) === (1: Term))
    assert(rewriter.searchStep(1: Term) === Bottom)
    assert(rewriter.searchStep(el ~~ 3 ~~ 4 ~~ 5 ~~ 6) ===
      Or(List(el ~~ 4 ~~ 0 ~~ 5, el ~~ 0 ~~ 4 ~~ 5, el ~~ 4 ~~ 5 ~~ 0)))
  }

  "contexts" - {

    "zero-level" in {
      assertRewrite(foo(a, AnywhereContext(X, Rewrite(Y, bar(Y)))))(
        foo(a, b),
        foo(a, bar(b)))
    }

    "a bit more" in {
      assertRewrite(foo(a, AnywhereContext(X, Rewrite(Y, bar(Y)))))(
        foo(a, traversed(b)),
        Or(
          foo(a, traversed(bar(b))),
          foo(a, bar(traversed(b)))
        )
      )
    }

    "with traversal" in {
      assertRewrite(foo(a, AnywhereContext(X, Rewrite(matched(Y), bar(Y)))))(
        foo(a, traversed(matched(andMatchingY()))),
        foo(a, traversed(bar(andMatchingY()))))
    }

    "with traversal outer rewrite" in {
      assertRewrite(foo(a, Rewrite(AnywhereContext(X, matched(Y)), bar(Y))))(
        foo(a, traversed(matched(andMatchingY()))),
        foo(a, bar(andMatchingY())))
    }

    "referring to context" in {
      assertRewrite(foo(a, Rewrite(AnywhereContext(X, matched(Y)), bar(X))))(
        foo(a, traversed(matched(andMatchingY()))),
        foo(a, bar(traversed(X_1))))
    }
  }

  "of pattern contexts" - {

    val XX = Variable("XX")
    val YY = Variable("YY")

    "zero level" in {
      assertRewrite(CAPP(C, Rewrite(1, 2)))(1, 2)
    }

    "one level" in {
      assertRewrite(CAPP(C, Rewrite(bar(X), X)))(bar(1), 1)
    }

    "two levels" in {
      assertRewrite(CAPP(C, Rewrite(bar(X), buz(X, X))))(
        foo(1, bar(bar(2))),
        Or(foo(1, buz(bar(2), bar(2))), foo(1, bar(buz(2, 2)))))
    }

    "stops traversal when encountering unknown" in {
      assertRewrite(CAPP(C, Rewrite(bar(X), buz(X, X))))(
        foo(1, bar(buz(3, bar(2)))),
        foo(1, buz(buz(3, bar(2)), buz(3, bar(2)))))
    }
  }

  "inline rewrite" - {
    "very simple" in {
      val rw = Rewrite(1, 2)
      println(rw := 1)
    }
  }
}
