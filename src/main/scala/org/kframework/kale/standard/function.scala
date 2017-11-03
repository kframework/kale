package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.{NameFromObject, LabelNamed}

trait FunctionByRewritingMixin extends Mixin {
  _: Environment with standard.MatchingLogicMixin with HasMatcher =>

  case class FunctionDefinedByRewritingMatcher(solver: Apply) extends Binary.F({ (a: Term, b: Term) => {
    val l = a.label.asInstanceOf[FunctionDefinedByRewriting]
    And(b, And(a.children.zip(b.children).map({
      case (ca, cb) => solver(ca, cb) match {
        case And.SPN(s, p, _) => And(s, p, Top)
        case Bottom => Bottom
      }
    })))
  }
  })

  register(Binary.definePartialFunction({
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FunctionDefinedByRewritingMatcher
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FunctionDefinedByRewritingMatcher
  }))
}

case class NotLabel()(implicit override val env: Environment) extends LabelNamed("¬") with kale.NotLabel with FunctionLabel {

  import env._

  override def apply(_1: Term): Term = f(_1) getOrElse SimpleNot(_1)

  def f(_1: Term): Option[Term] = _1 match {
    case `Top` =>
      Some(Bottom)
    case `Bottom` =>
      Some(Top)
    case Or.set(terms) if terms.size > 1 =>
      Some(And(terms map (Not(_))))
    case And.set(terms) if terms.size > 1 =>
      Some(Or(terms map (Not(_))))
    case _ => None
  }

  override val isPredicate: Option[Boolean] = Some(false)
}

case class SimpleNot(_1: Term)(implicit val env: Environment) extends Node1 {
  override val label: kale.NotLabel = env.Not
  override lazy val isPredicate: Boolean = _1.isPredicate
}

trait FunctionDefinedByRewriting extends FunctionLabel with PureFunctionLabel with NodeLabel {

  implicit val env: StandardEnvironment
  private var p_rewriter: Option[Term] = None

  def rewriter: Term = p_rewriter.get

  //throw new AssertionError("Set rules before sealing the environment. Or at least before trying to create new terms in the sealed environment.")

  def setRules(rules: Term): Unit = {
    p_rewriter = Some(rules)
  }

  def tryToApply(res: Term): Option[Term] =
    if (env.isSealed && p_rewriter.isDefined) {
      import env._
      // do not try to execute the function before the env is sealed as it would trigger the lazy initialization fo the Rewriter,
      // and a Rewriter can only be built once the Environment is sealed
      unify(rewriter, res) match {
        case Bottom => None
        case r =>
          Some(And.anytimeIsNow(OneResult(And.nextOnly(And.onlyNonPredicate(r)))))
      }
    } else {
      None
    }
}

case class FunctionDefinedByRewritingLabel0(name: String)
                                           (implicit val env: StandardEnvironment)
  extends FunctionDefinedByRewriting with FunctionLabel0 {

  def f(): Option[Term] = tryToApply(FreeNode0(this))
}

case class FunctionDefinedByRewritingLabel1(name: String)(implicit val env: StandardEnvironment) extends FunctionDefinedByRewriting with FunctionLabel1 {
  def f(_1: Term): Option[Term] = tryToApply(FreeNode1(this, _1))
}

case class FunctionDefinedByRewritingLabel2(name: String)(implicit val env: StandardEnvironment) extends FunctionDefinedByRewriting with FunctionLabel2 {
  def f(_1: Term, _2: Term): Option[Term] = tryToApply(FreeNode2(this, _1, _2))
}

case class FunctionDefinedByRewritingLabel3(name: String)(implicit val env: StandardEnvironment) extends FunctionDefinedByRewriting with FunctionLabel3 {
  def f(_1: Term, _2: Term, _3: Term): Option[Term] = tryToApply(FreeNode3(this, _1, _2, _3))
}

case class FunctionDefinedByRewritingLabel4(name: String)(implicit val env: StandardEnvironment) extends FunctionDefinedByRewriting with FunctionLabel4 {
  def f(_1: Term, _2: Term, _3: Term, _4: Term): Option[Term] = tryToApply(FreeNode4(this, _1, _2, _3, _4))
}
