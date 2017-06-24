package org.kframework.kale.standard

import com.typesafe.scalalogging.Logger
import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.builtin._
import org.kframework.kale.context.anywhere.AnywhereContextApplicationLabel
import org.kframework.kale.km.Z3Stuff
import org.kframework.kale.pretty.importPrettyWrapper

object StandardEnvironment {
  def apply(): StandardEnvironment = new StandardEnvironment with Z3Stuff {}
}

trait StandardEnvironment extends MatchingLogic with importBOOLEAN with importINT with importDOUBLE with importSTRING with importID with importPrettyWrapper with strategy.importSTRATEGY with AC {
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

  override def sort(l: Label, children: Seq[Term]): kale.Sort = Sort.K

  override def sortTarget(l: Label): kale.Sort = Sort.K

  override def sortArgs(l: Label): Seq[kale.Sort] = l match {
    case l: LeafLabel[_] => Seq()
    case l: NodeLabel => Seq.fill(l.arity)(Sort.K)
  }

  override val substitutionMaker: (Substitution) => SubstitutionApply = new SubstitutionWithContext(_)(this)

  protected override lazy val unifier = SingleSortedMatcher()(this)

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

  def log = Logger("EnvironmentLogger" + this.hashCode())
}
