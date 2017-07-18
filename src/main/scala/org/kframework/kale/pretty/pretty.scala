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
        And(Equality(a, t), a)
  })

  case class PrettyWrapperPrettyWrapper(solver: Apply) extends Binary.F({
    (a: PrettyWrapperHolder, b: PrettyWrapperHolder) => FreeNode3FreeNode3(solver)(a, b)
  })

  case class TermPrettyWrapper(solver: Apply) extends Binary.F({ (t: Term, a: PrettyWrapperHolder) =>
    solver(t, a.content).asOr map {
      case And.SPN(s, p, n) => And.SPN(s, p, a.copy(a._1, n, a._3))
      case Bottom => Bottom
    }
  })

  register(Binary.definePartialFunction({
    case (PrettyWrapper, PrettyWrapper) =>
      PrettyWrapperPrettyWrapper
    case (PrettyWrapper, term) =>
      PrettyWrapperTerm
    case (term, PrettyWrapper) =>
      TermPrettyWrapper
  }), Priority.high + 1)

  case class PrettyWrapperHolder(prefix: Term, content: Term, suffix: Term) extends Node3 {
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

  lazy val W = this
  lazy val I = Infer

  import STRING.String

  override def apply(_1: Term, _2: Term, _3: Term): Term = {
    _2 match {
      case assoc: Assoc =>
        val terms = assoc.assocIterable
        assoc.label(terms map {
          case t if t == terms.head => W(_1, t, I)
          case t if t == terms.last => W(I, t, _3)
          case t => t
        })
      case PrettyWrapper(_1inner, _2inner, _3inner) =>
        val _1res = mergeSpacing(_1, _1inner)
        val _3res = mergeSpacing(_3, _3inner)
        PrettyWrapper(_1res, _2inner, _3res)
      case o =>
        PrettyWrapperHolder(_1, _2, _3)
    }
  }

  private def mergeSpacing(_1: Term, _1inner: Term) = {
    (_1, _1inner) match {
      case (String(s1), String(s2)) => String(s1 + s2)
      case (Infer, String(s)) => String(s)
      case (String(s), Infer) => String(s)
      case (Infer, Infer) => Infer
    }
  }

  def unwrap(t: Term) = t match {
    case PrettyWrapper(_, c, _) => c
    case _ => t
  }

  val Infer = FreeLabel0("")()

  def wrapInInferTD(t: Term): Term = t match {
    case PrettyWrapper(p, c, s) =>
      PrettyWrapper(p, c map0 wrapInInferTD, s)
    case o if shouldBePretty(t) =>
      PrettyWrapper(Infer, o map0 wrapInInferTD, Infer)
    case o =>
      o map0 wrapInInferTD
  }

  /**
    * None means that it depends on its children
    */
  override val isPredicate: Option[Boolean] = Some(false)
}
