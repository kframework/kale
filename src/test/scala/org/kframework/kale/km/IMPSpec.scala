package org.kframework.kale.km

import org.kframework.kale.IMP.env
import org.kframework.kale._
import org.kframework.kale.standard.{FreeLabel2, Sort}
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

  val X = Variable("X", Sorts.Id)
  val I = Variable("I", Sorts.Int)
  val I1 = Variable("I1", Sorts.Int)
  val I2 = Variable("I2", Sorts.Int)
  val S = Variable("S", Sorts.StateMap)
  val SO = Variable("SO", Sorts.StateMap)
  val R = Variable("R", Sorts.KSeq)

  val rules = Set(
    T(k(kseq(Rewrite(X, I), R)), state(statesMap(varBinding(X, I), SO))),
    T(k(kseq(Rewrite(div(I1, I2), intDiv(I1, I2)), R)), S)
  ) map (t => Rewrite(lhs(t), rhs(t)))

  env.seal()

  val unify = new MultiSortedUnifier(env)

  "first test" in {
    assert(unify(X, ID("foo")) === Equality(X, ID("foo")))
    assert(unify(X, INT(2)) === Bottom)
  }
}