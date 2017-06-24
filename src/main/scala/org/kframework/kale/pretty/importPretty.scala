package org.kframework.kale.pretty

import org.kframework.kale._
import org.kframework.kale.builtin._
import org.kframework.kale.standard.Sort
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.Named

import scala.runtime.ScalaRunTime

trait PrettyMixin extends Mixin with Environment with standard.MatchingLogicMixin with standard.FreeMixin with builtin.StringMixin {

  val PrettyWrapper: PrettyWrapperLabel = new PrettyWrapperLabel()

  def pretty(t: Term): String

  class PrettyWrapperLabel extends Named("œ") with Label3 {

    override def apply(_1: Term, _2: Term, _3: Term): Term = {
      _2 match {
        case PrettyWrapper(STRING.String(_1inner), _2, STRING.String(_3inner)) =>
          val STRING.String(_1outer) = _1
          val STRING.String(_3outer) = _3
          PrettyWrapper(STRING.String(_1outer + _1inner), _2, STRING.String(_3inner + _3outer))
        case o =>
          PrettyWrapperHolder(this, _1, _2, _3)
      }
    }

    val self = this

    def unwrap(t: Term) = t match {
      case self(_, c, _) => c
      case _ => t
    }

    def unwrapBU(t: Term): Term = t mapBU unwrap
  }

  implicit class PrettyTerm(t: Term) {
    def pretty: String = PrettyMixin.this.pretty(t)
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
    t match {
      case v: Variable if v.sort == Sort("WhiteSpace") =>
        SortedVarLeft(solver)(v, a)
      case _ =>
        solver(t, a.content).asOr map {
          case And.withNext(p, Some(Next(n))) => And.withNext(p, Next(a.copy(a._1, n, a._3)))
          case Bottom => Bottom
        }
    }
  })

  override def makeMatcher = Binary.definePartialFunction({
    case (PrettyWrapper, PrettyWrapper) =>
      PrettyWrapperPrettyWrapper
    case (PrettyWrapper, term) =>
      PrettyWrapperTerm
    case (term, PrettyWrapper) =>
      TermPrettyWrapper
  }).orElse(super.makeMatcher)

  case class PrettyWrapperHolder(label: PrettyWrapperLabel, prefix: Term, content: Term, suffix: Term) extends Node3 with IsNotPredicate {
    override def toString =
      if (_1.toString.nonEmpty || _3.toString.nonEmpty)
        "⦅" + _1.toString + "|" + _2 + "|" + _3 + "⦆"
      else
        _2.toString

    override def _1: Term = prefix

    override def _2: Term = content

    override def _3: Term = suffix
  }

}
