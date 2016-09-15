package org.kframework.kale.transformer

import org.kframework.kale.{Environment, Label, Term}

import scala.collection.Set

object Binary {

  trait ProcessingFunction[Left <: Term, Right <: Term, Result <: Term] extends (State => ((Term, Term) => Term)) {
    def apply(solver: State) = { (a: Term, b: Term) => f(solver)(a.asInstanceOf[Left], b.asInstanceOf[Right]) }

    def f(solver: State)(a: Left, b: Right): Result
  }

  trait State {
    def apply(left: Term, right: Term): Term
  }

  /**
    * f specifies how to process a pair of terms with labels (leftLabel, rightLabel).
    * f is automatically hooked and applied via Apply.
    */
  case class Piece(leftLabel: Label, rightLabel: Label, f: State => (Term, Term) => Term)

  class Apply(pieces: Set[Piece], env: Environment) extends State {

    import env._

    val arr: Array[Array[(Term, Term) => (Term)]] =
      (0 until env.labels.size + 1).map({ i =>
        new Array[(Term, Term) => (Term)](env.labels.size + 1)
      }).toArray

    for (p <- pieces) {
      arr(p.leftLabel.id)(p.rightLabel.id) = p.f(this)
    }

    def apply(left: Term, right: Term): Term = {
      //      assert(labels.contains(left.label) && labels.contains(right.label))
      assert(left.label.id <= env.labels.size, "Left label " + left.label + " with id " + left.label.id + " is not registered. Label list:" + env.labels.map(l => (l.id, l)).toList.sortBy(_._1).mkString("\n"))
      assert(right.label.id <= env.labels.size, "Right label " + right.label + " with id " + right.label.id + " is not registered. Label list:" + env.labels.map(l => (l.id, l)).toList.sortBy(_._1).mkString("\n"))

      try {
        val u = arr(left.label.id)(right.label.id)
        val res = if (u != null)
          u(left, right)
        else
          Bottom

        assert(!(left == right && res == Bottom), left.toString)
        res
      } catch {
        case _: IndexOutOfBoundsException => throw new AssertionError("No processing function registered for: " + left + " and " + right)
      }
    }
  }

}
