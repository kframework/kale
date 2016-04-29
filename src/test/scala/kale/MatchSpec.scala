package kale

import org.scalatest.FreeSpec

import collection._
import scala.language.implicitConversions

class MatchSpec extends FreeSpec {

  import Implicits._

  val X = Variable("X")
  val Y = Variable("Y")

  val emptyList = FreeLabel0(UniqueId(), ".List")
  val listLabel = AssocWithIdListLabel("_,_", emptyList())

  val unifier = new Dispatch(Set(
    UnifierPiece(Variable, INT, SimpleMatcher.VarLeft),
    UnifierPiece(INT.+, INT.+, SimpleMatcher.FreeNode2FreeNode2),
    UnifierPiece(INT, INT, SimpleMatcher.Constants),
    UnifierPiece(listLabel, listLabel, SimpleMatcher.AssocTerm),
    UnifierPiece(listLabel, INT, SimpleMatcher.AssocTerm),
    UnifierPiece(INT, listLabel, SimpleMatcher.TermAssoc),
    UnifierPiece(listLabel, INT.+, SimpleMatcher.AssocTerm),
    UnifierPiece(INT.+, listLabel, SimpleMatcher.TermAssoc),
    UnifierPiece(listLabel, emptyList, SimpleMatcher.AssocTerm),
    UnifierPiece(emptyList, listLabel, SimpleMatcher.TermAssoc),
    UnifierPiece(listLabel, Variable, SimpleMatcher.AssocTerm),
    UnifierPiece(Variable, listLabel, SimpleMatcher.TermAssoc)
  ), 30)

  val substitutionApplier = SubstitutionApplication(Set(
    UnaryPiece(Variable, ApplySubstitution.Var),
    UnaryPiece(INT.+, ApplySubstitution.Node2),
    UnaryPiece(INT, ApplySubstitution.Constant),
    UnaryPiece(emptyList, ApplySubstitution.FreeNode0),
    UnaryPiece(listLabel, ApplySubstitution.Node2)
  ), 30) _

  "simple" - {
    assert(unifier(X, 5) === Equality(X, 5))
    assert(unifier(X + Y, (5: Term) + 7) === Substitution(Map(X -> (5: Term), Y -> (7: Term))))
    assert(unifier(X + X, (5: Term) + 7) === Bottom)
    assert(unifier((5: Term) + 7, (5: Term) + 7) === Top)
    //    assert((2: Term).unify(5: Term) == Bottom)
    //    assert((2: Term).unify(2: Term) == Top)
  }

  "assoc" - {
    assert(unifier(listLabel(X, 5), listLabel(3, 5)) === Equality(X, 3))
    assert(unifier(listLabel(List(3, 4, X, 7)), listLabel(List(3, 4, 5, 6, 7))) === Equality(X, listLabel(5, 6)))
    assert(unifier(listLabel(List(3, X, 5, Y, 7)), listLabel(List(3, 4, 5, 6, 7))) === Substitution(Map(X -> (4: Term), Y -> (6: Term))))
    assert(unifier(listLabel(List(X, 5, Y)), listLabel(List(3, 4, 5, 6, 7))) === Substitution(Map(X -> listLabel(3, 4), Y -> listLabel(6, 7))))
    val res = unifier(listLabel(List(3, X, Y, 6)), listLabel(List(3, 4, 5, 6)))
    println(res)
    assert(unifier(listLabel(List(3, X, Y, 6)), listLabel(List(3, 4, 5, 6))) ===
      Or(List(
        Substitution(Map(X -> emptyList(), Y -> listLabel(4, 5))),
        Substitution(Map(X -> (4:Term), Y -> (5:Term))),
        Substitution(Map(X -> listLabel(4, 5), Y -> emptyList()))))
    )
  }

  "substitution" - {
    val s = Substitution(Map(X -> (5: Term)))
    val substitution = substitutionApplier(s)

    assert(substitution(5) === (5: Term))
    assert(substitution(X) === (5: Term))
    assert(substitution(Y) === Y)
    assert(substitution(Y + X) === Y + 5)
  }

  "rewrite X + 0 => X" - {
    assert(substitutionApplier(unifier(X + 0, (5: Term) + 0).asInstanceOf[PureSubstitution])(X) === (5: Term))
  }

  "rewrite 2 + X + 3 => 5 + X" - {
    assert(substitutionApplier(unifier((2: Term) + X + 3, (2: Term) + 4 + 3).asInstanceOf[PureSubstitution])((5: Term) + X) === (5: Term) + 4)
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
