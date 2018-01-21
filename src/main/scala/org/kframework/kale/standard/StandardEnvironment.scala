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
  extends Environment
    with MatchingLogicMixin
    with HolesMixin
    with FreeMixin
    with TuplesMixin
    with builtin.BooleanMixin
    with builtin.IntMixin
    with builtin.DoubleMixin
    with builtin.StringMixin
    with builtin.IdMixin
    with ACMixin
    with AssocWithIdListMixin
    with NonAssocWithIdListMixin
    with standard.FunctionByRewritingMixin
    with builtin.MapMixin
    with BundledContextMixin
    with MacroMixin
    with strategy.StrategyMixin
    with ScalaLibraryMixin
    with PathMixin
    with MatchingLogicPostfixMixin {

  val Match = new MatchLabel()

  val ApplyRewrite = new GroundApplyRewrite()

  val OneResult = new OneResult()

  val ApplySimpleRewrite = new Compose2("ApplySimpleRewrite", ApplyRewrite, OneResult)

  override lazy val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionWithContext(_)

  final val unify: Label2 = lift("unify", {
    (a: Term, b: Term) =>
      assert(this.isSealed)
      val res = unifier(a, b)
      strongBottomize(res)({
        val And.SPN(sub, pred, nonPred) = res
        val newSub: Term = sub.asMap.foldLeft(Top: Term)({
          case (Bottom, _) => Bottom
          case (acc: Substitution, (k: Variable, v: Term)) =>
            val newValue: Term = acc(v)
            if (newValue.containsInConstructor(k)) {
              Bottom
            } else {
              val updatedAcc = Equality.binding(k, newValue)(acc).asInstanceOf[Substitution]
              And.substitution(updatedAcc.asMap + (k -> newValue))
            }
          case _ => throw new AssertionError("We have a serious error in the code just above.")
        })

        newSub match {
          case Bottom => Bottom
          case s: Substitution =>
            val newPred = s(pred)
            strongBottomize(newPred) {
              And.SPN(s, newPred, s(nonPred))
            }
        }
      })
  })

  protected[this] def unifier = matcher

  protected[this] def matcher = {
    if (_matcher == null)
      throw new AssertionError("Seal environment to have access to the matcher")
    _matcher
  }

  override def seal(): Unit = {
    super.seal()
    _matcher = Binary.Apply(this.makeMatcher)
  }

  // HELPERS:

  def rewrite(rule: Term, obj: Term): Term = {
    And.anytimeIsNow(And.onlyNonPredicate(unify(rule, obj)))
  }
}

trait NoSortingMixin extends Environment {
  def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.Top

  def sort(l: Label): Sort = Sort.Top

  override def isSort(left: kale.Sort, term: Term): Boolean = true
}

trait HolesMixin extends Mixin {
  _: Environment =>
  val Hole = Variable("☐", Sort.K)
  val Hole1 = Variable("☐1", Sort.K)
  val Hole2 = Variable("☐2", Sort.K)
  val Hole3 = Variable("☐3", Sort.K)
}

case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
  AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)
