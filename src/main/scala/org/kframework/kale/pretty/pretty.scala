package org.kframework.kale.pretty

import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.LabelNamed

trait PrettyWrapperMixin extends Mixin {
  _: Environment with standard.MatchingLogicMixin with standard.FreeMixin with builtin.StringMixin =>

  def pretty(t: Term): String = t match {
    case PrettyWrapper(p, c, s) => p + pretty(c) + s
    case _ => t.toString
  }

  val PrettyWrapper: Label3 = new LabelNamed("PrettyWrapper") with Label3 with Constructor {
    lazy private val W = this
    lazy val I = Infer

    override def apply(_1: Term, _2: Term, _3: Term): Term = bottomize(_2) {
      assert(isValidWrapper(_1), "Invalid wrapper left: " + _1)
      assert(isValidWrapper(_1), "Invalid: wrapper right" + _1)

      _2 match {
        case assoc: Assoc =>
          val terms = assoc.assocIterable
          assoc.label(terms map {
            case t if t == terms.head && !t.isPredicate =>
              t match {
                case PrettyWrapper(a, t, b) => PrettyWrapper(STRING.strconcat(_1, a), t, b)
                case _ => PrettyWrapper(_1, t, I)
              }
            case t if t == terms.last && !t.isPredicate =>
              t match {
                case PrettyWrapper(a, t, b) => PrettyWrapper(a, t, STRING.strconcat(b, _3))
                case _ => PrettyWrapper(I, t, _3)
              }
            case t => t
          })
        case PrettyWrapper(_1inner, _2inner, _3inner) =>
          val _1res = STRING.strconcat(_1, _1inner)
          val _3res = STRING.strconcat(_3inner, _3)
          PrettyWrapper(_1res, _2inner, _3res)
        case o =>
          PrettyWrapperHolder(_1, _2, _3)
      }
    }

    /**
      * None means that it depends on its children
      */
    override val isPredicate: Option[Boolean] = Some(false)

    override def requiredLabels(children: Iterable[Term]) = children.tail.head.requiredLabels

    override def suppliedLabels(children: Iterable[Term]) = Roaring.suppliedBy(children)
  }

  val Infer = FreeLabel0("I")()

  def wrapInInferTD(t: Term): Term = t match {
    case PrettyWrapper(p, c, s) =>
      PrettyWrapper(p, c map0 wrapInInferTD, s)
    case o if shouldBePretty(t) =>
      PrettyWrapper(Infer, o map0 wrapInInferTD, Infer)
    case o =>
      o map0 wrapInInferTD
  }

  def unwrapFromPrettyWrapper(t: Term) = t match {
    case PrettyWrapper(_, c, _) => c
    case _ => t
  }

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

  def isValidWrapper(_1: Term) = {
    _1 match {
      case STRING.String(s) => s.trim == ""
      case o => !o.isGround || o.exists(t => t.label == Rewrite || t == Infer || t.label == Next)
    }
  }

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