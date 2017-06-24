package org.kframework.kale.standard

import com.typesafe.scalalogging.Logger
import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.builtin._
import org.kframework.kale.context.anywhere.AnywhereContextApplicationLabel
import org.kframework.kale.km.{MultisortedMixing, Z3Stuff}
import org.kframework.kale.pretty.PrettyMixin

import scala.collection.Seq

object StandardEnvironment {
  def apply(): StandardEnvironment = new StandardEnvironment with NoSortingMixin {
  }
}

trait StandardEnvironment extends MatchingLogicMixin with FreeMixin with builtin.BooleanMixin with builtin.IntMixin with builtin.DoubleMixin with builtin.StringMixin with builtin.IdMixin with PrettyMixin with strategy.StrategyMixin with ACMixin with standard.FunctionByRewritingMixin with builtin.MapMixin {
  val Hole = Variable("☐", Sort.K)
  val Hole1 = Variable("☐1", Sort.K)
  val Hole2 = Variable("☐2", Sort.K)
  val Hole3 = Variable("☐3", Sort.K)

  val BindMatch = new BindMatchLabel()

  val Match = new MatchLabel()

  val ApplyRewrite = new GroundApplyRewrite()

  val OneResult = new OneResult()

  val ApplySimpleRewrite = new Compose2("ApplySimpleRewrite", ApplyRewrite, OneResult)

  val AnywhereContext = AnywhereContextApplicationLabel()

  def ANYWHERE(t: Term) = AnywhereContext(Variable.freshVariable, t)

  override lazy val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionWithContext(_)(this)

  override lazy val unifier = matcher
  lazy val matcher = new SingleSortedMatcher(this.makeMatcher)

  def pretty(t: Term): String = t match {
    case PrettyWrapper(p, c, s) => p + pretty(c) + s
    case _ => t.toString
  }

  // HELPERS:

  def rewrite(rule: Term, obj: Term): Term = {
    unify(rule, obj).asOr map {
      case And.withNext(p, Some(Next(t))) =>
        if (!p.exists(_.label == Not)) {
          t
        } else {
          log.warn("Rewriter rule didn't apply because it's not clear if we can prove a Not");
          Bottom
        }

      case Bottom => Bottom
    }
  }
}

trait NoSortingMixin extends Environment {
  def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.Top
  def sort(l: Label): Sort = Sort.Top
}