package kale

import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec {
  import LOGIC._
  import Implicits._

  "simple" - {
    val X = Variable("X")
    assert(X.unify(5: Term) == Substitutions.Pair(X, 5))
    assert((2: Term).unify(5: Term) == False)
    assert((2: Term).unify(2: Term) == True)
  }

  "LIST" in {
    val l = new ASSOC_LIST("_,_", 0)
    println(l.op(1, 2))
  }
}
