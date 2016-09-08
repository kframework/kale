package org.kframework.kale.transformer

import org.kframework.kale.{Label, Term}

import scala.collection.Set

/**
  * Abstract stateful transformer from Term to Term
  *
  * To use, define a TransformationFunction for each relevant label, and hook it by instantiating Pieces and
  * passing them to an implementation of Apply.
  */
object Unary {

  /**
    * Extend this class to define the transformation by implementing f.
    */
  trait TransformationFunction[Element <: Term, Result <: Term, US <: State] extends (US => Term => Term) {
    def apply(unaryState: US) = { t: Term => f(unaryState)(t.asInstanceOf[Element]) }

    def f(state: US)(t: Element): Result
  }

  trait State {
    def apply(t: Term): Term
  }

  /**
    * Transformation f is automatically hooked and applied to the label.
    */
  case class Piece[US <: State](label: Label, f: US => Term => Term)

  abstract class Apply[US <: State](pieces: Set[Piece[US]], maxId: Int) extends State {
    this: US =>

    val arr = new Array[Term => Term](maxId)

    for (p <- pieces) {
      arr(p.label.id) = p.f(this)
    }

    def apply(t: Term): Term
  }

}
