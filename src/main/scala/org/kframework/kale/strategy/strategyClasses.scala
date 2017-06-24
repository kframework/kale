package org.kframework.kale.strategy

import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.{ProcessingFunctions, definePartialFunction}
import org.kframework.kale.util.Named
import org.kframework.kale.{Environment, FreeNode1, FreeNode2, HasMatcher, Label1, Label2, Mixin, Term, standard}

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

  val STRATEGY = org.kframework.kale.strategy.STRATEGY()

  import STRATEGY._

  override protected def makeMatcher: ProcessingFunctions = definePartialFunction({
    case (`orElse`, _) => orElseTerm
    case (`compose`, _) => composeTerm
    case (`repeat`, _) => repeatTerm
    case (`fixpoint`, _) => fixpointTerm
  }).orElse(super.makeMatcher)

  // only works for ground obj
  case class orElseTerm(solver: Binary.Apply) extends Binary.F({ (orElse: Term, obj: Term) =>
    val STRATEGY.orElse(theThen, theElse) = orElse

    obj.asOr map { t =>
      val thenSol = unify(theThen, t)
      thenSol match {
        case Bottom => unify(theElse, t)
        case other => other
      }
    }
  })

  case class composeTerm(solver: Binary.Apply) extends Binary.F({ (composed: Term, obj: Term) =>
    val compose(f, g) = composed
    val matchG = unify(g, obj)
    val takeRelevantFromGMatch = nextIsNow(onlyNext(matchG))
    val matchF = unify(f, takeRelevantFromGMatch)

    matchF
  })

  case class repeatTerm(solver: Binary.Apply) extends Binary.F({ (fp: Term, obj: Term) =>
    val repeat(f) = fp
    val someVar = Variable.freshVariable()
    val sol = solver(orElse(f, someVar), obj)
    sol.asOr map {
      case And.withNext(p, Some(Next(t))) =>
        if (p.contains(someVar)) {
          Next(t)
        } else {
          solver(fp, t) // TODO: pass in the remaining predicates
        }
    }
  })

  case class fixpointTerm(solver: Binary.Apply) extends Binary.F({ (fp: Term, obj: Term) =>
    val fixpoint(f) = fp
    solver(f, obj) match {
      case Bottom => Bottom
      case Next(`obj`) => Next(obj)
      case res => solver(fp, And.nextIsNow(res))
    }
  })

}
