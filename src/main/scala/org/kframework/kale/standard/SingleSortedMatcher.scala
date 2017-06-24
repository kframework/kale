package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.context.anywhere.AnywhereContextMatcher
import org.kframework.kale.context.pattern.{PatternContextApplicationLabel, PatternContextMatcher}
import org.kframework.kale.transformer.Binary

case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
  AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)


class SingleSortedMatcher(input: Binary.ProcessingFunctions)(implicit val env: StandardEnvironment) extends Binary.Apply {

  import Binary._
  import env._

  override def apply(left: Term, right: Term): Term = {
    val res = super.apply(left, right)
    //    assert(env.Or.asSet(res).forall({
    //      case env.And.withNext(_, Some(_)) => true
    //    }))
    res
  }

  case class IfThenElseTerm(solver: Apply) extends Binary.F({ (a: Node3, b: Term) =>
    val c = solver(a._1, b)
    if (c == Bottom)
      a._3
    else
      And(c, a._2)
  })

  case class BindMatchMatcher(solver: Apply) extends Binary.F({ (a: Node2, b: Term) =>
    val v = a._1.asInstanceOf[Variable]
    val p = a._2
    b.asOr map { bx =>
      val sol = solver(p, bx)
      And(Equality(v, bx), sol)
    }
  }
  )

  case class RewriteMatcher(solver: SingleSortedMatcher) extends Binary.F({ (a: SimpleRewrite, b: Term) =>
    val env = solver.env
    import env._
    val m = solver(a._1, b)
    m.asOr map {
      case And.withNext(nonNext@And.substitutionAndTerms(subs, terms), _) =>
        val s = substitutionMaker(subs)
        And(Next(s(a._2)), nonNext)
    }
  }
  )

  import standard._

  override def processingFunctions: ProcessingFunctions =
      definePartialFunction({
        case (`Rewrite`, _) => RewriteMatcher
        case (`BindMatch`, _) => BindMatchMatcher
        case (`AnywhereContext`, _) => new AnywhereContextMatcher()(env)
        case (capp: PatternContextApplicationLabel, _) => new PatternContextMatcher()(env)
      }).orElse(input)
}
