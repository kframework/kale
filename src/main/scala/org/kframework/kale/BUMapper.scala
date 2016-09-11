package org.kframework.kale

import org.kframework.kale.transformer.Unary

import scala.collection.Set

object BUMapper {
  def apply(pieces: Set[Unary.Piece[BUMapper]], env: Environment)(func: PartialFunction[Term, Term]): BUMapper = new BUMapper(pieces, env)(func)

  def apply(env: Environment): PartialFunction[Term, Term] => BUMapper = {
    import env._

    val setOfUnaryPieces = labels.map({
      case `Variable` => Unary.Piece(Variable, Identity)
      case l: Label0 => Unary.Piece(l, Identity)
      case l: Label1 => Unary.Piece(l, Node1)
      case l: Label2 => Unary.Piece(l, Node2)
      case l: Label3 => Unary.Piece(l, Node3)
      case l: Label4 => Unary.Piece(l, Node4)
      case l: Label5 => Unary.Piece(l, Node5)
      case l: Label6 => Unary.Piece(l, Node6)
      case l: ConstantLabel[_] => Unary.Piece(l, Identity)
    })

    BUMapper(setOfUnaryPieces, env)
  }

  object Identity extends Unary.ProcessingFunction[Term, Term, BUMapper] {
    def f(solver: BUMapper)(t: Term) = t
  }

  object Node1 extends Unary.ProcessingFunction[Node1, Term, BUMapper] {
    def f(solver: BUMapper)(t: Node1) = t.label(solver(t._1))
  }

  object Node2 extends Unary.ProcessingFunction[Node2, Term, BUMapper] {
    def f(solver: BUMapper)(t: Node2) = t.label(solver(t._1), solver(t._2))
  }

  object Node3 extends Unary.ProcessingFunction[Node3, Term, BUMapper] {
    def f(solver: BUMapper)(t: Node3) = t.label(solver(t._1), solver(t._2), solver(t._3))
  }

  object Node4 extends Unary.ProcessingFunction[Node4, Term, BUMapper] {
    def f(solver: BUMapper)(t: Node4) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

  object Node5 extends Unary.ProcessingFunction[Node5, Term, BUMapper] {
    def f(solver: BUMapper)(t: Node5) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5))
  }

  object Node6 extends Unary.ProcessingFunction[Node6, Term, BUMapper] {
    def f(solver: BUMapper)(t: Node6) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4), solver(t._5), solver(t._6))
  }

}

class BUMapper(val pieces: Set[Unary.Piece[BUMapper]], val env: Environment)(val func: PartialFunction[Term, Term]) extends Unary.Apply[BUMapper](pieces, env) with (Term => Term) {
  val liftedF = func.lift

  def apply(t: Term) =
    arr(t.label.id) match {
      case f =>
        val processedT = f(t)
        liftedF(processedT).getOrElse(processedT)
    }
}
