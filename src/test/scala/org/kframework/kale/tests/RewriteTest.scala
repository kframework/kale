package org.kframework.kale.tests

import org.kframework.kale._
import org.scalatest.FreeSpec

import scala.collection._
import scala.language.implicitConversions

class RewriteTest extends FreeSpec with TestSetup {

  import env._
  import implicits._

  "X + 0 => X" in {
    assert(substitutionApplier(unifier(X + 0, (5: Term) + 0).asInstanceOf[Substitution])(X) === (5: Term))
  }

  "2 + X + 3 => 5 + X" in {
    assert(substitutionApplier(unifier((2: Term) + X + 3, (2: Term) + 4 + 3).asInstanceOf[Substitution])((5: Term) + X) === (5: Term) + 4)
  }

  val rewriter = Rewriter(substitutionApplier, unifier, env)(Set(
    Rewrite(X + 0, X),
    Rewrite((0: Term) + X, X),
    Rewrite(listLabel(3, X, Y, 6), listLabel(X, 0, Y))
  ))

  "step" in {
    assert(rewriter.executionStep((1: Term) + 0) === (1: Term))
    assert(rewriter.executionStep(1: Term) === Bottom)
  }

  "search" in {
    assert(rewriter.searchStep((1: Term) + 0) === (1: Term))
    assert(rewriter.searchStep(1: Term) === Bottom)
    assert(rewriter.searchStep(listLabel(3, 4, 5, 6)) ===
      Or(List(listLabel(4, 0, 5), listLabel(0, 4, 5), listLabel(4, 5, 0))))
  }
}
