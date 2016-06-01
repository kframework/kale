package org.kframework.kale

import org.kframework.kale.transformer.Unary

import scala.collection.Set

object Mapper {
  def apply(pieces: Set[Unary.Piece[Mapper]], maxId: Int)(func: Term => Term): Mapper = new Mapper(pieces, maxId)(func)

  def apply(labels: Set[Label]): (Term => Term) => Mapper = {
    val maxId = labels.map(_.id).max + 1
    val setOfUnaryPieces = labels.map({
      case `Variable` => Unary.Piece(Variable, Identity)
      case l: Label0 => Unary.Piece(l, Identity)
      case l: Label1 => Unary.Piece(l, Node1)
      case l: Label2 => Unary.Piece(l, Node2)
      case l: Label3 => Unary.Piece(l, Node3)
      case l: Label4 => Unary.Piece(l, Node4)
      case l: ConstantLabel[_] => Unary.Piece(l, Identity)
    })

    Mapper(setOfUnaryPieces, maxId)
  }

  object Identity extends Unary.Function[Term, Term, Mapper] {
    def f(solver: Mapper)(t: Term) = t
  }

  object Node1 extends Unary.Function[Node1, Term, Mapper] {
    def f(solver: Mapper)(t: Node1) = t.label(solver(t._1))
  }

  object Node2 extends Unary.Function[Node2, Term, Mapper] {
    def f(solver: Mapper)(t: Node2) = t.label(solver(t._1), solver(t._2))
  }

  object Node3 extends Unary.Function[Node3, Term, Mapper] {
    def f(solver: Mapper)(t: Node3) = t.label(solver(t._1), solver(t._2), solver(t._3))
  }

  object Node4 extends Unary.Function[Node4, Term, Mapper] {
    def f(solver: Mapper)(t: Node4) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

}

class Mapper(val pieces: Set[Unary.Piece[Mapper]], val maxId: Int)(val func: Term => Term) extends Unary.Apply[Mapper](pieces, maxId) with (Term => Term) {
  def apply(t: Term) =
    arr(t.label.id) match {
      case null => func(t)
      case f => func(f(t))
    }
}
