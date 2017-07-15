package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.context.BundledContextMixin
import org.kframework.kale.transformer.Binary

import scala.collection.Seq

object StandardEnvironment {
  def apply(): StandardEnvironment = new StandardEnvironment with NoSortingMixin
}

trait StandardEnvironment
  extends MatchingLogicMixin
    with HolesMixin
    with FreeMixin
    with TuplesMixing
    with builtin.BooleanMixin
    with builtin.IntMixin
    with builtin.DoubleMixin
    with builtin.StringMixin
    with builtin.IdMixin
    with AssocWithIdListMixin
    with NonAssocWithIdListMixing
    with standard.FunctionByRewritingMixin
    with builtin.MapMixin
    with BundledContextMixin
    with MacroMixin
    with strategy.StrategyMixin
    with MatchingLogicPostfixMixin {

  val Match = new MatchLabel()

  val ApplyRewrite = new GroundApplyRewrite()

  val OneResult = new OneResult()

  val ApplySimpleRewrite = new Compose2("ApplySimpleRewrite", ApplyRewrite, OneResult)

  override lazy val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionWithContext(_)

  override lazy val unifier = matcher

  lazy val matcher = Binary.Apply(this.makeMatcher)


  // HELPERS:

  def rewrite(rule: Term, obj: Term): Term = {
    And.nextIsNow(And.onlyNonPredicate(unify(rule, obj)))
  }
}

trait NoSortingMixin extends Environment {
  def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.Top

  def sort(l: Label): Sort = Sort.Top

  override def isSort(left: org.kframework.kore.Sort, term: Term): Boolean = true
}

trait HolesMixin extends MatchingLogicMixin {
  val Hole = Variable("☐", Sort.K)
  val Hole1 = Variable("☐1", Sort.K)
  val Hole2 = Variable("☐2", Sort.K)
  val Hole3 = Variable("☐3", Sort.K)
}

case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
  AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)
