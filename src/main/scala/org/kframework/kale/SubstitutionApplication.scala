package org.kframework.kale

import org.kframework.kale.transformer.Unary

import scala.collection.Set

object SubstitutionApplication {
  def apply(pieces: Set[Unary.Piece[SubstitutionApplication]], maxId: Int)(s: Substitution) = new SubstitutionApplication(pieces, maxId)(s)

  def apply(labels: Set[Label]): Substitution => SubstitutionApplication = {
    val maxId = labels.map(_.id).max + 1
    val setOfUnaryPieces = labels.map({
      case `Variable` => Unary.Piece(Variable, Var)
      case l: Label0 => Unary.Piece(l, Node0)
      case l: Label1 => Unary.Piece(l, Node1)
      case l: Label2 => Unary.Piece(l, Node2)
      case l: Label3 => Unary.Piece(l, Node3)
      case l: Label4 => Unary.Piece(l, Node4)
      case l: ConstantLabel[_] => Unary.Piece(l, Constant)
    })

    SubstitutionApplication(setOfUnaryPieces, maxId)
  }

  object Node0 extends Unary.Function[Node0, Node0, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node0) = t
  }

  object Node1 extends Unary.Function[Node1, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node1) = t.label(solver(t._1))
  }

  object Node2 extends Unary.Function[Node2, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node2) = t.label(solver(t._1), solver(t._2))
  }

  object Node3 extends Unary.Function[Node3, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node3) = t.label(solver(t._1), solver(t._2), solver(t._3))
  }

  object Node4 extends Unary.Function[Node4, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node4) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

  object Var extends Unary.Function[Variable, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(v: Variable) = solver.get(v).getOrElse(v)
  }

  object Constant extends Unary.Function[Constant[_], Constant[_], SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(a: Constant[_]) = a
  }
}

class SubstitutionApplication(pieces: Set[Unary.Piece[SubstitutionApplication]], maxId: Int)(s: Substitution) extends Unary.Application[SubstitutionApplication](pieces, maxId) {
  def get(v: Variable): Option[Term] = s.get(v)

  def apply(t: Term) = arr(t.label.id) match {
    case null => Bottom
    case f => f(t)
  }
}