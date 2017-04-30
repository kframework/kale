package org.kframework.kale.transformer

import org.kframework.kale._
import org.kframework.kale.transformer.GenUnary.Apply
import org.kframework.kale.util.Util

/**
  * Abstract stateful transformer from Term to Term
  *
  * To use, define a TransformationFunction for each relevant label, and hook it by instantiating Pieces and
  * passing them to an implementation of Apply.
  */
object GenUnary {

  object ProcessingFunction {
    implicit def functionToProcessingFunction[PassInType <: Term, ReturnType, SpecificSolver <: Apply[ReturnType]](func: SpecificSolver => PassInType => ReturnType): ProcessingFunction[ReturnType, SpecificSolver] = new ProcessingFunction[ReturnType, SpecificSolver] {
      override type Element = PassInType

      override def f(state: SpecificSolver)(t: Element): ReturnType = func(state)(t)
    }
  }

  /**
    * Extend this class to define the transformation by implementing f.
    */
  trait ProcessingFunction[ReturnType, -SpecificSolver <: Apply[ReturnType]] extends (SpecificSolver => Term => ReturnType) {
    type Element <: Term

    def apply(unarySolver: SpecificSolver): (Term => ReturnType) = { t: Term => f(unarySolver)(t.asInstanceOf[Element]) }

    def f(state: SpecificSolver)(t: Element): ReturnType
  }


  abstract class Apply[T](env: Environment) extends (Term => T) {

    type ProcessingFunctions = PartialFunction[Label, ProcessingFunction[T, this.type]]

    protected def definePartialFunction(f: ProcessingFunctions): ProcessingFunctions = f

    protected def processingFunctions: ProcessingFunctions

    protected lazy val arr: Array[Term => T] = {
      val pf = processingFunctions

      val a = new Array[Term => T](env.labels.size + 1)
      for (label <- env.labels) {
        a(label.id) = pf(label)(this)
      }
      a
    }

    def apply(t: Term): T = arr(t.label.id)(t)
  }

}

object Unary {
  type ProcessingFunction[-SpecificSolver <: Apply] = GenUnary.ProcessingFunction[Term, SpecificSolver]

  def Node0(solver: Apply)(t: Node0): Term = t.copy()

  def Node1(solver: Apply)(t: Node1): Term = t.copy(solver(t._1))

  def Node2(solver: Apply)(t: Node2): Term = t.copy(solver(t._1), solver(t._2))

  def Node3(solver: Apply)(t: Node3): Term = t.copy(solver(t._1), solver(t._2), solver(t._3))

  def Node4(solver: Apply)(t: Node4): Term = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4))

  def Node5(solver: Apply)(t: Node5): Term = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5))

  def Node6(solver: Apply)(t: Node6): Term = t.copy(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5), solver(t._6))

  def DoNothing(solver: Apply)(a: Term): Term = a


  abstract class Apply(env: Environment) extends GenUnary.Apply[Term](env) {

    protected def processingFunctions: ProcessingFunctions = {
      case l: Label0 => Node0 _
      case l: Label1 => Node1 _
      case l: Label2 => Node2 _
      case l: Label3 => Node3 _
      case l: Label4 => Node4 _
      case l: Label5 => Node5 _
      case l: Label6 => Node6 _
      case l: LeafLabel[_] => DoNothing _
    }

    def fixpoint(t: Term): Term = Util.fixpoint(apply)(t)
  }

}