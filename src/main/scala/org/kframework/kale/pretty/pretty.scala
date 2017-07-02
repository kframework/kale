package org.kframework.kale.pretty

import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.Named

trait PrettyWrapperMixin extends Mixin with Environment with standard.MatchingLogicMixin with standard.FreeMixin with builtin.StringMixin {

  def pretty(t: Term): String = t match {
    case PrettyWrapper(p, c, s) => p + pretty(c) + s
    case _ => t.toString
  }

  val PrettyWrapper = new PrettyWrapperLabel()

  implicit class PrettyTerm(t: Term) {
    def pretty: String = PrettyWrapperMixin.this.pretty(t)
  }

  case class PrettyWrapperTerm(solver: Apply) extends Binary.F({
    (a: PrettyWrapperHolder, t: Term) =>
      if (t.isGround)
        Bottom
      else
        And.withNext(Equality(a, t), Next(a))
  })

  case class PrettyWrapperPrettyWrapper(solver: Apply) extends Binary.F({
    (a: PrettyWrapperHolder, b: PrettyWrapperHolder) => FreeNode3FreeNode3(solver)(a, b)
  })

  case class TermPrettyWrapper(solver: Apply) extends Binary.F({ (t: Term, a: PrettyWrapperHolder) =>
    solver(t, a.content).asOr map {
      case And.withNext(p, Some(Next(n))) => And.withNext(p, Next(a.copy(a._1, n, a._3)))
      case Bottom => Bottom
    }
  })

  override protected def makeMatcher = Binary.definePartialFunction({
    case (PrettyWrapper, PrettyWrapper) =>
      PrettyWrapperPrettyWrapper
    case (PrettyWrapper, term) =>
      PrettyWrapperTerm
    case (term, PrettyWrapper) =>
      TermPrettyWrapper
  }).orElse(super.makeMatcher)

  case class PrettyWrapperHolder(prefix: Term, content: Term, suffix: Term) extends Node3 with IsNotPredicate {
    override def toString =
      if (_1.toString.nonEmpty || _3.toString.nonEmpty)
        "⦅" + _1.toString + "|" + _2 + "|" + _3 + "⦆"
      else
        _2.toString

    override def _1: Term = prefix

    override def _2: Term = content

    override def _3: Term = suffix

    override val label: Label3 = PrettyWrapper
  }

  def shouldBePretty(term: Term): Boolean
}

class PrettyWrapperLabel(implicit eenv: Environment with StringMixin with PrettyWrapperMixin) extends Named("PrettyWrapper") with Label3 {

  import env._

  override def apply(_1: Term, _2: Term, _3: Term): Term = {
    _2 match {
      case PrettyWrapper(STRING.String(_1inner), _2, STRING.String(_3inner)) =>
        val STRING.String(_1outer) = _1
        val STRING.String(_3outer) = _3
        PrettyWrapper(STRING.String(_1outer + _1inner), _2, STRING.String(_3inner + _3outer))
      case o =>
        PrettyWrapperHolder(_1, _2, _3)
    }
  }

  def unwrap(t: Term) = t match {
    case PrettyWrapper(_, c, _) => c
    case _ => t
  }

  val Infer = FreeLabel0("InferWhiteSpace")()

  def wrapInInferTD(t: Term): Term = t match {
    case PrettyWrapper(p, c, s) =>
      PrettyWrapper(p, c map0 wrapInInferTD, s)
    case o if shouldBePretty(t) =>
      PrettyWrapper(Infer, o map0 wrapInInferTD, Infer)
    case o =>
      o map0 wrapInInferTD
  }
}