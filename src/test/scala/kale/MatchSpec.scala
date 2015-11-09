package kale

import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec {

  import LOGIC._
  import Implicits._

  val X = Variable("X")
  val Y = Variable("Y")

  "simple" - {
    assert(X.unify(5: Term) == Substitutions.Pair(X, 5))
    assert((2: Term).unify(5: Term) == False)
    assert((2: Term).unify(2: Term) == True)
  }

  "LIST" in {
    val l = new ASSOC_LIST("_,_", 0)
    val x = l.op(1, 2)
    val y = l.op(3, 4)
    assert(x.unify(x) == True)
    assert(x.unify(y) == False)
//    val z = Substitutions.Pair(X, x)
//    assert(X.unify(x) == Substitutions.Pair(X, x))
//    assert(l.op(X, 2).unify(x) == Substitutions.Pair(X, 1))
//    assert(l.op(X, Y).unify(x) == Or(
//      And(And(And(X, 1), And(Y, 2)),
//        And(And(X, 0), And(Y, l.op(1, 2)))),
//      And(And(X, l.op(1, 2)), And(Y, 0))))
  }
}
