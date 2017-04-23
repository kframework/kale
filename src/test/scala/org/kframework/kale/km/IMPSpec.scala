package org.kframework.kale.km

import org.kframework.kale.IMP.env
import org.kframework.kale._
import org.kframework.kale.standard.{FreeLabel2, FreeLabel3, Sort}
import org.kframework.kale.util.Implicits
import org.scalatest.FreeSpec

import scala.collection.Seq

class IMPSpec extends FreeSpec {
  implicit val env = new KMEnvironment()

  import env._

  val signature = new IMPCommonSignature()

  import signature._

  val ints = FreeLabel2("_,_")

  object Sorts {
    val Pgm = Sort("Pgm")
    val Id = Sort("Id")
    val Ids = Sort("Ids")
    val Int = Sort("Int")
    val AExp = Sort("AExp")
    val BExp = Sort("BExp")
    val Block = Sort("Block")
    val Stmt = Sort("Stmt")
    val StateMap = Sort("StateMap")
    val KSeq = Sort("KSeq")
    val Cell = Sort("Cell")
    val K = Sort("K")
    val IntList = Sort("IntList")
  }

  {
    import Sorts._

    sorted(ID, Id)

    sorted(INT, Int)

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

    sorted(ints, IntList, IntList, IntList)
  }


  val kseq = FreeLabel2("_~>_")
  sorted(kseq, Sorts.K, Sorts.KSeq, Sorts.KSeq)

  // TODO: testing purpose only
  val ppp = FreeLabel3("ppp")
  sorted(ppp, Sorts.Id, Sorts.Id, Sorts.Id, Sorts.K)

  val X = Variable("X", Sorts.Id)
  val Y = Variable("Y", Sorts.Id)
  val I = Variable("I", Sorts.Int)
  val I1 = Variable("I1", Sorts.Int)
  val I2 = Variable("I2", Sorts.Int)
  val S = Variable("S", Sorts.StateMap)
  val SO = Variable("SO", Sorts.StateMap)
  val R = Variable("R", Sorts.KSeq)

  val E1 = Variable("E1", Sorts.AExp)
  val E2 = Variable("E2", Sorts.AExp)
  val E3 = Variable("E3", Sorts.AExp)

  val rules = Set(
    T(k(kseq(Rewrite(X, I), R)), state(statesMap(varBinding(X, I), SO))),
    T(k(kseq(Rewrite(div(I1, I2), intDiv(I1, I2)), R)), S)
  ) map (t => Rewrite(lhs(t), rhs(t)))

  env.seal()

  val unify = new MultiSortedUnifier(env)

  "first test" in {
    assert(unify(X, ID("foo")) === Equality(X, ID("foo")))
    assert(unify(X, Y) === Equality(X, Y))
    assert(unify(X, INT(2)) === Bottom)

    assert(unify(plus(E1,E2), leq(E1,E2)) == Bottom)

    assert(unify(plus(E1,E2), plus(E2,E1)) == Equality(E1, E2))
//  assert(unify(plus(E1,E2), plus(E2,E1)) == Equality(E2, E1)) // TODO: is that ok?

    println(
      unify(
        div(plus(E1,E2), plus(E2,E1)),
        div(E3, E3)
      )
    )
    // E3 = _+_(E1, E1) ∧ E2 = E1
    // original: E1 = E2, E3 = plus(E1,E2)

    println(
      unify(
        ppp(X, Y, ID("a")),
        ppp(Y, X, X)
      )
    )
    // X = a ∧ Y = a
    // original: X = Y, Y = a


    // TODO: negative tests (i.e., occur check)

  }
}