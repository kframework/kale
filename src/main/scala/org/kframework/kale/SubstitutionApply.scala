package org.kframework.kale

import org.kframework.kale.transformer.Unary

import scala.collection.Set

object SubstitutionApply {
  def apply(pieces: Set[Unary.Piece[SubstitutionApply]], maxId: Int)(s: Substitution): SubstitutionApply = new SubstitutionApply(pieces, maxId)(s)

  def apply(labels: Set[Label]): Substitution => SubstitutionApply = {
    val maxId = labels.map(_.id).max + 1
    val setOfUnaryPieces = labels.map({
      case `Variable` => Unary.Piece(Variable, Var)
      case l: Context1Label => Unary.Piece(l, Context1)

      case l: Label0 => Unary.Piece(l, Node0)
      case l: Label1 => Unary.Piece(l, Node1)
      case l: Label2 => Unary.Piece(l, Node2)
      case l: Label3 => Unary.Piece(l, Node3)
      case l: Label4 => Unary.Piece(l, Node4)
      case l: ConstantLabel[_] => Unary.Piece(l, Constant)
    })

    SubstitutionApply(setOfUnaryPieces, maxId)
  }

  object Node0 extends Unary.Function[Node0, Node0, SubstitutionApply] {
    def f(solver: SubstitutionApply)(t: Node0) = t
  }

  object Node1 extends Unary.Function[Node1, Term, SubstitutionApply] {
    def f(solver: SubstitutionApply)(t: Node1) = t.label(solver(t._1))
  }

  object Node2 extends Unary.Function[Node2, Term, SubstitutionApply] {
    def f(solver: SubstitutionApply)(t: Node2) = t.label(solver(t._1), solver(t._2))
  }

  object Node3 extends Unary.Function[Node3, Term, SubstitutionApply] {
    def f(solver: SubstitutionApply)(t: Node3) = t.label(solver(t._1), solver(t._2), solver(t._3))
  }

  object Node4 extends Unary.Function[Node4, Term, SubstitutionApply] {
    def f(solver: SubstitutionApply)(t: Node4) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

  object Var extends Unary.Function[Variable, Term, SubstitutionApply] {
    def f(solver: SubstitutionApply)(v: Variable) = solver.getVariable(v).getOrElse(v)
  }

  object Constant extends Unary.Function[Constant[_], Constant[_], SubstitutionApply] {
    def f(solver: SubstitutionApply)(a: Constant[_]) = a
  }

  object Context1 extends Unary.Function[Context1, Term, SubstitutionApply] {
    override def f(solver: SubstitutionApply)(t: Context1): Term = {
      Substitution(solver.s, Equality(t.hole, solver(t.term))) match {
        case subs: Substitution =>
          val innerSolver: SubstitutionApply = SubstitutionApply(solver.pieces, solver.maxId)(subs)

          solver.getVariable(t.contextVar) map innerSolver getOrElse Bottom
        case `Bottom` => Bottom
      }
    }
  }

}

class SubstitutionApply(val pieces: Set[Unary.Piece[SubstitutionApply]], val maxId: Int)(val s: Substitution) extends Unary.Apply[SubstitutionApply](pieces, maxId) with (Term => Term) {
  def getVariable(v: Variable): Option[Term] = s.get(v)

  def apply(t: Term) = arr(t.label.id) match {
    case null => Bottom
    case f => f(t)
  }
}