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
    "mixed" in {
      assert(orElse(Rewrite(a, c), Rewrite(b, d)).unify(Or(a, b)) === Or(Next(c), Next(d)))
    }
  }

  "nextIsNow" in {
    assert(nextIsNow(And(Next(a), Equality(X, a))) === And(a, Equality(X, a)))
  }

  "compose" in {
    assert(compose(Rewrite(b, c), Rewrite(a, b)).unify(a) === Next(c))
  }

  "repeat" - {
    val repeatRule = repeat(Or(Rewrite(a, b), Rewrite(b, c)))
    "simple" in {
      assert(repeatRule.unify(a) === Next(c))
    }
    "disjunction" in {
      assert(repeatRule.unify(Or(a, d)) === Or(Next(c), Next(d)))
    }
  }

  "fixpoint" - {
    val fp = fixpoint(Or(Rewrite(a, b), Rewrite(b, b)))
    "simple" in {
      assert(fp.unify(a) === Next(b))
      assert(fp.unify(b) === Next(b))
      assert(fp.unify(c) === Bottom)
    }
    "disjunction" in {
      assert(fp.unify(Or(a, d)) === Next(b))
    }
  }
}
