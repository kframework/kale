package org.kframework.kale.pretty

import org.kframework.kale.Term
import org.kframework.kale.standard.{NoSortingMixin, StandardEnvironment}
import org.kframework.kale.tests.TestSetup
import org.scalatest.FreeSpec

object TestEnv extends StandardEnvironment with PrettyWrapperMixin with NoSortingMixin {
  def shouldBePretty(term: Term) = true
}

class PrettyWrapperTest extends TestSetup()(TestEnv) {
  private def assertRewrite(rule: Term, input: Term, expected: Term) = {
    val actual = rule rewrite input
    assert(actual == expected)
  }

  import implicits._
  import env._

  val A = listLabel

  val rw = Rewrite

  val W = PrettyWrapper

  "pretty" - {
    val three = PrettyWrapper("a", 3, "b")
    "ground" in {

      assert(three.pretty === "a3b")

      assert(((3: Term) := three) === Top)

      assert((X := three) === Equality(X, 3))
    }

    val fooThree = PrettyWrapper("c", foo(three, 6), "d")

    "wrapper left" in {
      assert(And(foo(X, 6) :== fooThree, Equality(X, 3)) === And(Equality(X, 3), Next(fooThree)))
    }

    "wrapper right" in {
      assert(And(fooThree := foo(X, 6), Equality(X, 3)) === Bottom)
    }

    "wrapper wrapper" in {
      val fooThreeDifferent = PrettyWrapper("e", foo(three, 6), "f")
      assert((fooThree := fooThree) == Top)
      assert((fooThree := fooThreeDifferent) == Bottom)
    }
  }

  "assoc rewrite" in {
    assertRewrite(rw(u, v), W("1", u, "2"), W("1", v, "2"))
    assertRewrite(A(rw(el, v), a), W("1", a, "2"), W("1", A(v, a), "2"))
    assertRewrite(A(a, rw(el, v)), W("1", a, "2"), W("1", A(a, v), "2"))
    assertRewrite(A(rw(el, v), a, b), W("p", A(W("1", a, "2"), W("3", b, "4")), "s"), W("p", A(v, W("1", a, "2"), W("3", b, "4")), "s"))
    assertRewrite(A(a, rw(el, v), b), W("p", A(W("1", a, "2"), W("3", b, "4")), "s"), W("p", A(W("1", a, "2"), v, W("3", b, "4")), "s"))
    assertRewrite(A(a, b, rw(el, v)), W("p", A(W("1", a, "2"), W("3", b, "4")), "s"), W("p", A(W("1", a, "2"), W("3", b, "4"), v), "s"))
  }

  "assoc rewrite with var boundary" in {
    assertRewrite(rw(u, v), W("1", u, "2"), W("1", v, "2"))
    assertRewrite(A(rw(el, v), X), W("1", a, "2"), W("1", A(v, a), "2"))
    assertRewrite(A(X, rw(el, v)), W("1", a, "2"), W("1", A(a, v), "2"))
    assertRewrite(A(rw(el, v), X), W("p", A(W("1", a, "2"), W("3", b, "4")), "s"), W("p", A(v, W("1", a, "2"), W("3", b, "4")), "s"))

    assertRewrite(A(X, rw(el, v), Y), W("p", A(W("1", a, "2"), W("3", b, "4")), "s"),
      Or(
        W("p", A(v, W("1", a, "2"), W("3", b, "4")), "s"),
        W("p", A(W("1", a, "2"), v, W("3", b, "4")), "s"),
        W("p", A(W("1", a, "2"), W("3", b, "4"), v), "s")
      )
    )

    assertRewrite(A(X, rw(el, v)), W("p", A(W("1", a, "2"), W("3", b, "4")), "s"), W("p", A(W("1", a, "2"), W("3", b, "4"), v), "s"))
  }
}
