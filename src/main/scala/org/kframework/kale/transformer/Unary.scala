package org.kframework.kale.transformer

import org.kframework.kale._
import org.kframework.kale.util.Util

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
  trait ProcessingFunction[-SpecificSolver <: Apply] extends (SpecificSolver => Term => Term) {
    type Element <: Term

    def apply(unarySolver: SpecificSolver): (Term => Term) = { t: Term => f(unarySolver)(t.asInstanceOf[Element]) }

    def f(state: SpecificSolver)(t: Element): Term
  }


  object Node0 extends Unary.ProcessingFunction[Apply] {
    type Element = Node0

    def f(solver: Apply)(t: Node0) = t.copy()
  }

  object Node1 extends Unary.ProcessingFunction[Apply] {
    type Element = Node1

    def f(solver: Apply)(t: Node1) = t.copy(solver(t._1))
  }

  object Node2 extends Unary.ProcessingFunction[Apply] {
    type Element = Node2

    def f(solver: Apply)(t: Node2) = t.copy(solver(t._1), solver(t._2))
  }

  object Node3 extends Unary.ProcessingFunction[Apply] {
    type Element = Node3

    def f(solver: Apply)(t: Node3) = t.copy(solver(t._1), solver(t._2), solver(t._3))
  }

  object Node4 extends Unary.ProcessingFunction[Apply] {
    type Element = Node4

    def f(solver: Apply)(t: Node4) = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

  object Node5 extends Unary.ProcessingFunction[Apply] {
    type Element = Node5

    def f(solver: Apply)(t: Node5) = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5))
  }

  object Node6 extends Unary.ProcessingFunction[Apply] {
    type Element = Node6

    def f(solver: Apply)(t: Node6) = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5), solver(t._6))
  }

  object DoNothing extends Unary.ProcessingFunction[Apply] {
    type Element = Term

    def f(solver: Apply)(a: Term) = a
  }


  abstract class Apply(env: Environment) extends (Term => Term) {

    type ProcessingFunctions = PartialFunction[Label, ProcessingFunction[this.type]]

    protected def definePartialFunction(f: ProcessingFunctions): ProcessingFunctions = f

    protected def processingFunctions: ProcessingFunctions = {
      case l: Label0 => Node0
      case l: Label1 => Node1
      case l: Label2 => Node2
      case l: Label3 => Node3
      case l: Label4 => Node4
      case l: Label5 => Node5
      case l: Label6 => Node6
      case l: LeafLabel[_] => DoNothing
    }

    protected lazy val arr: Array[(Term) => Term] = {
      val pf = processingFunctions

      val a = new Array[Term => Term](env.labels.size + 1)
      for (label <- env.labels) {
        a(label.id) = pf(label)(this)
      }
      a
    }

    def apply(t: Term): Term = arr(t.label.id)(t)

    def fixpoint(t: Term): Term = Util.fixpoint(apply)(t)
  }

}
