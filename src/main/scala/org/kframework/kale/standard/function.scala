package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.util.{NameFromObject, Named}

import scala.collection.Set


class InvokeLabel(implicit val env: Environment) extends NameFromObject with Label1 {
  // the rewriter is initialized after the creation of the label to break the cycle when creating the rewriter for applying functions
  var rewriter: Rewriter = _

  override def apply(obj: Term): Term = env.bottomize(obj) {
    Invoke(this, obj)
  }
}

case class Invoke(label: InvokeLabel, _1: Term) extends Node1 {
  override lazy val isPredicate: Boolean = ???
}

case class NotLabel(implicit override val env: Environment) extends Named("¬") with kale.NotLabel with FunctionLabel {

  import env._

  override def apply(_1: Term): Term = env.bottomize(_1) {
    f(_1) getOrElse SimpleNot(_1)
  }

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
}

case class SimpleNot(_1: Term)(implicit val env: Environment) extends Node1 {
  override val label: kale.NotLabel = env.Not
  override val isPredicate: Boolean = _1.isPredicate
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
        case r => Some(OneResult(And.onlyNext(r)))
      }
    } else {
      None
    }
}

case class FunctionDefinedByRewritingLabel0(name: String)(implicit val env: StandardEnvironment) extends FunctionDefinedByRewriting with FunctionLabel0 {
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

case class Macro1(name: String, rw: Term)(implicit val env: StandardEnvironment) extends Label1 {

  import env._

  def apply(_1: Term): Term = Equality.binding(Hole, _1)(rw)
}

case class Macro2(name: String, rw: Term)(implicit val env: StandardEnvironment) extends Label2 {

  import env._

  def apply(_1: Term, _2: Term): Term = And.substitution(Map(Hole1 -> _1, Hole2 -> _2))(rw)
}
