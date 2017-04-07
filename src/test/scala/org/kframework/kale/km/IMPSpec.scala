package org.kframework.kale.km

import org.kframework.kale.IMP.env
import org.kframework.kale.{IMPCommonSignature, Term, standard}
import org.kframework.kale.standard.{FreeLabel2, Sort}
import org.scalatest.FreeSpec

class IMPSpec extends FreeSpec {
  "first test" in {
    implicit val env = new KMEnvironment()
    import env._

    val signature = new IMPCommonSignature()
    import signature._

    val ints = FreeLabel2("_,_")
    val kseq = FreeLabel2("_~>_")

    object sort {
      val Id = Sort("Id")
      val Int = Sort("Int")
      val StateMap = Sort("StateMap")
      val KSeq = Sort("KSeq")
    }

    val X = Variable("X", sort.Id)
    val I = Variable("I", sort.Int)
    val I1 = Variable("I1", sort.Int)
    val I2 = Variable("I2", sort.Int)
    val S = Variable("S", sort.StateMap)
    val SO = Variable("SO", sort.StateMap)
    val R = Variable("R", sort.KSeq)

    val rules = Set(
      T(k(kseq(Rewrite(X, I), R)), state(statesMap(varBinding(X, I), SO))),
      T(k(kseq(Rewrite(div(I1, I2), intDiv(I1, I2)), R)), S)
    ) map (t => Rewrite(lhs(t), rhs(t)))

    env.seal()
  }
}