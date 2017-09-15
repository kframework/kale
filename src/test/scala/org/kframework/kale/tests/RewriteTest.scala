package org.kframework.kale.tests

import org.kframework.kale._
import org.kframework.kale.standard.StandardEnvironment
import org.scalatest.FreeSpec

import scala.language.implicitConversions

class RewriteTest extends TestSetup[StandardEnvironment]() {

  import env._
  import implicits._

  implicit val eeenv = env

  "X + 0 => X" in {
    assertRewrite(Rewrite(X + 0, X))((5: Term) + 0, 5: Term)
  }

  "2 + X + 3 => 5 + X" in {
    assertRewrite(Rewrite((2: Term) + X + 3, (5: Term) + X))((2: Term) + 4 + 3, (5: Term) + 4)
  }

  val rewriter = Or(
    Rewrite(X + 0, X),
    Rewrite((0: Term) + X, X),
    Rewrite(el ~~ 3 ~~ X ~~ Y ~~ 6, el ~~ X ~~ 0 ~~ Y)
  )

  "inner rewrite" - {
    "simple" in {
      assert((bar(Rewrite(X, b)) =:= bar(a)) === And(Equality(X, a), bar(Next(b))))
    }
    "swap" in {
      assert((foo(Rewrite(X, Y), Rewrite(Y, X)) =:= foo(a, b)) === And(Equality(X, a), Equality(Y, b), foo(Next(b), Next(a))))
    }
  }

  "step" in {
    assert((rewriter ==:= ((1: Term) + 0)) === Next(1))
    assert((rewriter ==:= (1: Term)) === Bottom)
  }

  "search" in {
    assert((rewriter ==:= ((1: Term) + 0)) === Next(1))
    assert((rewriter ==:= (1: Term)) === Bottom)
  }

  // TODO: check this test
  "search assoc" in {
    assert(And.nextIsNow(rewriter ==:= (el ~~ 3 ~~ 4 ~~ 5 ~~ 6)) ===
      Or(List(el ~~ 4 ~~ 0 ~~ 5, el ~~ 0 ~~ 4 ~~ 5, el ~~ 4 ~~ 5 ~~ 0)))
  }

  "nested disjunction" in {
    assertRewrite(And(Rewrite(A, A), Equality(A, Or(a, b))))(a, a)
  }

  "contexts" - {

    "zero-level" in {
      assertRewrite(foo(a, Context(X, Rewrite(Y, bar(Y)))))(
        foo(a, b),
        foo(a, bar(b)))
    }

    "a bit more" in {
      assertRewrite(foo(a, Context(X, Rewrite(Y, bar(Y)))))(
        foo(a, traversed(b)),
        Or(
          foo(a, traversed(bar(b))),
          foo(a, bar(traversed(b)))
        )
      )
    }

    "with traversal" in {
      assertRewrite(foo(a, Context(X, Rewrite(matched(Y), bar(Y)))))(
        foo(a, traversed(matched(andMatchingY()))),
        foo(a, traversed(bar(andMatchingY()))))
    }

    "with traversal outer rewrite" in {
      assertRewrite(foo(a, Rewrite(Context(X, matched(Y)), bar(Y))))(
        foo(a, traversed(matched(andMatchingY()))),
        foo(a, bar(andMatchingY())))
    }

    "referring to context" in {
      assertRewrite(foo(a, Rewrite(Context(X, matched(Y)), bar(X))))(
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
      assert((rw =:= 1) === Next(2))
    }
  }

  "flip" in {
    assert(Rewrite(foo(A, B), foo(B, A)).rewrite(foo(a, b)) === foo(b, a))
  }
}
