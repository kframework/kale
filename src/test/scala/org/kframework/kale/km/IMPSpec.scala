package org.kframework.kale.km

import org.kframework.kale._
import org.kframework.kale.standard.{SimpleFreeLabel2, SimpleFreeLabel3, Sort}
import org.scalatest.FreeSpec

class IMPSpec extends FreeSpec {
  implicit val env = new KMEnvironment()

  import env._

  // sort delcarations
  import Sort._

  object ImpSorts {
    val Id = Sort("Id")
    val Ids = Sort("Ids")
    val Int = Sort("Int")
    val IntList = Sort("IntList")
    //
    val AExp = Sort("AExp")
    val BExp = Sort("BExp")
    val Block = Sort("Block")
    val Stmt = Sort("Stmt")
    val Pgm = Sort("Pgm")
    //
    val Cell = Sort("Cell")
    val StateMap = Sort("StateMap")
    val KSeq = Sort("KSeq")
  }

  import ImpSorts._

  // sortify builtin symbols
  sorted(ID.Id, Id)
  sorted(INT.Int, Int)

  // import/sortify common symbols
  val signature = new IMPCommonSignature()

  import signature._

  sorted(div, AExp, AExp, AExp)
  sorted(plus, AExp, AExp, AExp)
  sorted(leq, AExp, AExp, BExp)
  sorted(not, BExp, BExp)
  sorted(and, BExp, BExp, BExp)
  sorted(emptyBlock, Block)
  sorted(block, Stmt, Block)
  sorted(assign, Id, AExp, Stmt)
  sorted(ifthenelse, BExp, Block, Block, Stmt)
  sorted(whiledo, BExp, Block, Stmt)
  sorted(seq, Stmt, Stmt, Stmt)
  sorted(program, Ids, Stmt, Pgm)
  sorted(T, Cell, Cell, Cell)
  sorted(k, K, Cell)
  sorted(state, StateMap, Cell)
  sorted(varBinding, Id, Int, StateMap)
  sorted(emptyIntList, IntList)
  sorted(emptyStates, StateMap)
  sorted(statesMap, StateMap, StateMap, StateMap)
  sorted(emptyk, K)

  // symbol declarations
  val ints = SimpleFreeLabel2("_,_");
  sorted(ints, IntList, IntList, IntList)
  val kseq = SimpleFreeLabel2("_~>_");
  sorted(kseq, K, KSeq, KSeq)
  // TODO: testing purpose only
  val ppp = SimpleFreeLabel3("ppp");
  sorted(ppp, Id, Id, Id, K)

  // variable declarations
  val X = Variable("X", Id)
  val Y = Variable("Y", Id)
  val I = Variable("I", Int)
  val I1 = Variable("I1", Int)
  val I2 = Variable("I2", Int)
  val E1 = Variable("E1", AExp)
  val E2 = Variable("E2", AExp)
  val E3 = Variable("E3", AExp)
  val S = Variable("S", StateMap)
  val SO = Variable("SO", StateMap)
  val R = Variable("R", KSeq)

  // semantics
  val rules = Set(
    T(k(kseq(Rewrite(X, I), R)), state(statesMap(varBinding(X, I), SO))),
    T(k(kseq(Rewrite(div(I1, I2), INT.div(I1, I2)), R)), S)
  ) map (t => Rewrite(lhs(t), rhs(t)))

  env.seal()

  // TODO(Daejun): move to unify test
  val unifier = new MultiSortedUnifier(env)

  def unify(a: Term, b: Term) = {
    unifier(a, b).asOr map {
      x => And(And.asSet(x).filter(_.label != Next))
    }
  }


  "first test" - {

    "simple" in {
      assert(unify(X, 'foo) === Equality(X, 'foo))
      assert(unify(X, Y) === Equality(X, Y))
      assert(unify(X, INT.Int(2)) === Bottom)

      assert(unify(plus(E1, E2), leq(E1, E2)) == Bottom)

      assert(unify(plus(E1, E2), plus(E2, E1)) == Equality(E1, E2))
      //  assert(unify(plus(E1,E2), plus(E2,E1)) == Equality(E2, E1)) // TODO: is that ok?
    }

    val a = 'a

    "div 1" in {

      assert(
        // q(p(x,y), p(y,x)) =?= q(z,z)
        unify(
          div(plus(E1, E2), plus(E2, E1)),
          div(E3, E3)
        )
          ==
          // E3 = _+_(E1, E1) ∧ E2 = E1
          And(
            Equality(E3, plus(E1, E2)),
            Equality(E2, E1)
          )
      )
    }

    "div 2" in {

      assert(
        // p(x,y,a) =?= p(y,x,x)
        unify(
          ppp(X, Y, a),
          ppp(Y, X, X)
        )
          ==
          // X = a ∧ Y = a
          And(
            Equality(X, a),
            Equality(Y, a)
          )
        /*
      // original: X = Y, Y = a
      And(
        Equality(X, Y),
        Equality(Y, a)
      )
       */
      )
    }

    "div 3" in {
      // negative test
      assert(
        // p(x,y) =?= x
        unify(
          plus(E1, E2),
          E1
        )
          ==
          // unification failure
          Bottom
      )
    }
  }
}
