package org.kframework.kale.strategy

import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.util.Named
import org.kframework.kale.{FreeNode1, FreeNode2, Label1, Label2, Term, standard}

case class STRATEGY()(implicit penv: StandardEnvironment) {

  val orElse = new Named("orElse") with Label2 {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  val nextIsNow = standard.lift("nextIsNow", penv.And.nextIsNow _)

  val onlyNext = standard.lift("onlyNext", penv.And.onlyNext _)

  val compose = new Named("compose") with Label2 {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  val repeat = new Named("repeat") with Label1 {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }

  val fixpoint = new Named("fixpoint") with Label1 {
    override def apply(f: Term): Term = FreeNode1(this, f)
  }
}

trait importSTRATEGY {
  protected val env: StandardEnvironment


  val STRATEGY = org.kframework.kale.strategy.STRATEGY()(env)
}
