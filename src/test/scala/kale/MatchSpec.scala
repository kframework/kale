package kale

import org.scalatest.FreeSpec

class MatchSpec extends FreeSpec {

  import Implicits._

  val X = Variable("X")
  val Y = Variable("Y")

  "simple" - {

    val pieces = Set(
      UnifierPiece(Variable, INT, SimpleMatcher.VarLeft),
      UnifierPiece(INT.+, INT.+, SimpleMatcher.FreeNode2FreeNode2)
    )
    val unifier = new Dispatch(pieces, 10)

    assert(unifier(X, 5) === Binding(X, 5))
    assert(unifier(X + Y, (5: Term) + 7) === Substitution(Map(X -> 5, Y -> 7)))
//    assert((2: Term).unify(5: Term) == Bottom)
//    assert((2: Term).unify(2: Term) == Top)
  }

//  "LIST" in {
//    val l = new ASSOC_LIST("_,_", 0)
//    val x = l.op(1, 2)
//    val y = l.op(3, 4)
//    assert(x.unify(x) == Top)
//    assert(x.unify(y) == Bottom)
//    val z = Substitutions.Pair(X, x)
//    assert(X.unify(x) == Substitutions.Pair(X, x))
//    assert(l.op(X, 2).unify(x) == Substitutions.Pair(X, 1))
//    assert(l.op(X, Y).unify(x) == Or(
//      And(And(And(X, 1), And(Y, 2)),
//        And(And(X, 0), And(Y, l.op(1, 2)))),
//      And(And(X, l.op(1, 2)), And(Y, 0))))
}
