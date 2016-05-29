package org.kframework.kale.transformer

import org.kframework.kale.{Label, Term}

import scala.collection.Set

object Unary {

  trait Function[Element <: Term, Result <: Term, US <: State] extends (US => Term => Term) {
    def apply(unaryState: US) = { t: Term => f(unaryState)(t.asInstanceOf[Element]) }

    def f(state: US)(t: Element): Result
  }

  trait State {
    def apply(t: Term): Term
  }

  case class Piece[US <: State](label: Label, f: US => Term => Term)

  abstract class Application[US <: State](pieces: Set[Piece[US]], maxId: Int) extends State {
    this: US =>
    val arr = new Array[Term => Term](maxId)

    for (p <- pieces) {
      arr(p.label.id) = p.f(this)
    }

    def apply(t: Term): Term
  }

}
