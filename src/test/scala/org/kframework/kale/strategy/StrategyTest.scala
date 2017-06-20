package org.kframework.kale.strategy

import org.kframework.kale.tests.TestSetup
import org.scalatest.FreeSpec

class StrategyTest extends FreeSpec with TestSetup {

  import env._
  import STRATEGY._

  "orElse" - {
    "then" in {
      assert(orElse(X, b).unify(a) === And(Next(a), Equality(X, a)))
    }
    "else" in {
      assert(orElse(a, Y).unify(b) === And(Next(b), Equality(Y, b)))
    }
  }

  "nextIsNow" in {
    assert(nextIsNow(And(Next(a), Equality(X, a))) === And(a, Equality(X, a)))
  }

  "functionReference" in {
    assert(functionReference(Rewrite(a, Hole)).unify(b).unify(a) === Next(b))
  }

  "compose" in {
    assert(compose(Rewrite(b, c), Rewrite(a, b)).unify(a) === Next(c))
  }

  "repeat" in {
    assert(repeat(Or(Rewrite(a, b), Rewrite(b, c))).unify(a) === Next(c))
  }

  "fixpoint" in {
    assert(fixpoint(Or(Rewrite(a, b), Rewrite(b, b))).unify(a) === Next(b))
  }
}
