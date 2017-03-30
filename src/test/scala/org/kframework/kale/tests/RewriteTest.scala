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
    Rewrite(el ~~ 3 ~~ X ~~ Y ~~ 6, el ~~ X ~~ 0 ~~ Y)
  ))

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
}
