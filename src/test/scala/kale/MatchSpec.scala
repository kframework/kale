package kale

import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec {
  "simple" - {
    import LOGIC._
    import Implicits._

    val X = Variable("X")
    assert(X.unify(5: Term) == Substitutions.Pair(X, 5))
    assert((2: Term).unify(5: Term) == False)
    assert((2: Term).unify(2: Term) == True)
  }
}
