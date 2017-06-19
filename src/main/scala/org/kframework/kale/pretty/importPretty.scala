package org.kframework.kale.pretty

import org.kframework.kale.builtin._
import org.kframework.kale.util.Named
import org.kframework.kale._

class PrettyWrapperLabel(implicit penv: Environment with hasPrettyWrapper with hasSTRING) extends Named("œ") with Label3 {

  import penv._

  override def apply(_1: Term, _2: Term, _3: Term): Term = {
    assert(_1.label == STRING.String)
    assert(_3.label == STRING.String)

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

trait importPrettyWrapper {
  protected val env: Environment with hasPrettyWrapper with hasSTRING

  val PrettyWrapper = new PrettyWrapperLabel()(env)

  def pretty(t: Term): String

  implicit class PrettyTerm(t: Term) {
    def pretty: String = importPrettyWrapper.this.pretty(t)
  }

}

