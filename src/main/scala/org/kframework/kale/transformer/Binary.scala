package org.kframework.kale.transformer

import org.kframework.kale.{Bottom, Label, Term}

import scala.collection.Set

object Binary {

  trait TransformationFunction[Left <: Term, Right <: Term, Result <: Term] extends (State => ((Term, Term) => Term)) {
    def apply(solver: State) = { (a: Term, b: Term) => f(solver)(a.asInstanceOf[Left], b.asInstanceOf[Right]) }

    def f(solver: State)(a: Left, b: Right): Result
  }

  trait State {
    def apply(left: Term, right: Term): Term
  }

  case class Piece(leftLabel: Label, rightLabel: Label, f: State => (Term, Term) => Term)

  class Apply(pieces: Set[Piece], labels: Set[Label]) extends State {
    val maxId = labels.map(_.id).max + 1

    val arr: Array[Array[(Term, Term) => (Term)]] =
      (0 until maxId + 1).map({ i =>
        new Array[(Term, Term) => (Term)](maxId)
      }).toArray

    for (p <- pieces) {
      arr(p.leftLabel.id)(p.rightLabel.id) = p.f(this)
    }

    def apply(left: Term, right: Term): Term = {
//      assert(labels.contains(left.label) && labels.contains(right.label))

      val u = arr(left.label.id)(right.label.id)
      val res = if (u != null)
        u(left, right)
      else
        Bottom

      assert(!(left == right && res == Bottom), left.toString)
      // println(left + "\n:= " + right + "\n=== " + res)
      res
    }
  }

}
