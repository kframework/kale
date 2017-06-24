package org.kframework.kale.strategy

import org.kframework.kale.standard.StandardEnvironment
import org.kframework.kale.transformer.Binary.{ProcessingFunctions, definePartialFunction}
import org.kframework.kale.util.Named
import org.kframework.kale.{Environment, FreeNode1, FreeNode2, HasMatcher, Label1, Label2, Mixin, Term, standard, strategy}

case class STRATEGY()(implicit env: Environment with standard.MatchingLogicMixin) {

  val orElse = new Named("orElse") with Label2 {
    override def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
  }

  val nextIsNow = standard.lift("nextIsNow", env.And.nextIsNow _)

  val onlyNext = standard.lift("onlyNext", env.And.onlyNext _)

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

trait StrategyMixin extends Mixin with Environment with standard.MatchingLogicMixin with HasMatcher {

  val STRATEGY = org.kframework.kale.strategy.STRATEGY()(env)

  import STRATEGY._

  override protected def makeMatcher: ProcessingFunctions = definePartialFunction({
    case (`orElse`, _) => strategy.orElseTerm
    case (`compose`, _) => strategy.composeTerm
    case (`repeat`, _) => strategy.repeatTerm
    case (`fixpoint`, _) => strategy.fixpointTerm
  }).orElse(super.makeMatcher)

}
