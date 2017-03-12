package org.kframework.kale.transformer

import org.kframework.kale.{Term, _}
import org.kframework.kale.context.Context1ApplicationLabel

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
  trait ProcessingFunction[-Element <: Term, US <: State] extends (US => Term => Term) {
    def apply(unaryState: US) = { t: Term => f(unaryState)(t.asInstanceOf[Element]) }

    def f(state: US)(t: Element): Term
  }

  trait State {
    def apply(t: Term): Term
  }


  def Node0[S <: State] = new Unary.ProcessingFunction[Node0, S] {
    def f(solver: S)(t: Node0) = t.copy()
  }

  def Node1[S <: State] = new Unary.ProcessingFunction[Node1, S] {
    def f(solver: S)(t: Node1) = t.copy(solver(t._1))
  }

  def Node2[S <: State] = new Unary.ProcessingFunction[Node2, S] {
    def f(solver: S)(t: Node2) = t.copy(solver(t._1), solver(t._2))
  }

  def Node3[S <: State] = new Unary.ProcessingFunction[Node3, S] {
    def f(solver: S)(t: Node3) = t.copy(solver(t._1), solver(t._2), solver(t._3))
  }

  def Node4[S <: State] = new Unary.ProcessingFunction[Node4, S] {
    def f(solver: S)(t: Node4) = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

  def Node5[S <: State] = new Unary.ProcessingFunction[Node5, S] {
    def f(solver: S)(t: Node5) = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5))
  }

  def Node6[S <: State] = new Unary.ProcessingFunction[Node6, S] {
    def f(solver: S)(t: Node6) = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5), solver(t._6))
  }

  def DoNothing[S <: State] = new Unary.ProcessingFunction[Term, S] {
    def f(solver: S)(a: Term) = a
  }

  def defaultMapping[US <: State]: PartialFunction[Label, ProcessingFunction[_ <: Term, US]] = {
    case l: Label0 => Node0
    case l: Label1 => Node1
    case l: Label2 => Node2
    case l: Label3 => Node3
    case l: Label4 => Node4
    case l: Label5 => Node5
    case l: Label6 => Node6
    case l: LeafLabel[_] => DoNothing
  }

  abstract class Apply[US <: State](env: Environment) extends State {
    this: US =>

    val arr = new Array[Term => Term](env.labels.size + 1)

    for (label <- env.labels) {
      arr(label.id) = processingFunction(label)(this)
    }

    def apply(t: Term): Term

    val processingFunction: Label => ProcessingFunction[_ <: Term, US]
  }

}
