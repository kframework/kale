package org.kframework.kale

import org.kframework.kale.transformer.Binary

import scala.collection.{Iterable, Set}

object Matcher {

  import context._
  import Binary._

  def apply(implicit env: Environment): Apply = {
    import env._

    object FreeNode0FreeNode0 extends ProcessingFunction[Node0, Node0, Top.type] {
      def f(solver: State)(a: Node0, b: Node0) = Top
    }

    object FreeNode1FreeNode1 extends ProcessingFunction[FreeNode1, FreeNode1, Term] {
      def f(solver: State)(a: FreeNode1, b: FreeNode1) = solver(a._1, b._1)
    }

    object FreeNode2FreeNode2 extends ProcessingFunction[FreeNode2, FreeNode2, Term] {
      def f(solver: State)(a: FreeNode2, b: FreeNode2) = And(solver(a._1, b._1), solver(a._2, b._2))
    }

    object FreeNode3FreeNode3 extends ProcessingFunction[FreeNode3, FreeNode3, Term] {
      def f(solver: State)(a: FreeNode3, b: FreeNode3) = And(List(solver(a._1, b._1), solver(a._2, b._2), solver(a._3, b._3)))
    }

    object FreeNode4FreeNode4 extends ProcessingFunction[FreeNode4, FreeNode4, Term] {
      def f(solver: State)(a: FreeNode4, b: FreeNode4) = And(List(solver(a._1, b._1), solver(a._2, b._2), solver(a._3, b._3), solver(a._4, b._4)))
    }

    def matchContents(l: AssocLabel, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: State): Term = {
      val res = (ksLeft.toSeq, ksRight.toSeq) match {
        case (Seq(), Seq()) => Top
        case ((v: Variable) +: tailL, ksR) =>
          (0 to ksR.size)
            .map { index => (ksR.take(index), ksR.drop(index)) }
            .map { case (prefix, suffix) => And(Equality(v, l(prefix)), matchContents(l, tailL, suffix)) }
            .fold(Bottom)({ (a, b) => Or(a, b) })
        case (left, right) if left.nonEmpty && right.nonEmpty => And(solver(left.head, right.head), matchContents(l, left.tail, right.tail): Term)
        case other => Bottom
      }
      res
    }

    object AssocTerm extends ProcessingFunction[Assoc, Term, Term] {
      def f(solver: State)(a: Assoc, b: Term) = {
        val asList = a.label.asList _
        val l1 = asList(a)
        val l2 = asList(b)
        matchContents(a.label, l1, l2)(solver)
      }
    }

    object TermAssoc extends ProcessingFunction[Term, Assoc, Term] {
      def f(solver: State)(a: Term, b: Assoc) = {
        val asList = b.label.asList _
        val l1 = asList(a)
        val l2 = asList(b)
        matchContents(b.label, l1, l2)(solver)
      }
    }

    object VarLeft extends ProcessingFunction[Variable, Term, Term] {
      def f(solver: State)(a: Variable, b: Term) = Equality(a.asInstanceOf[Variable], b)
    }

    object Constants extends ProcessingFunction[Constant[_], Constant[_], Term] {
      override def f(solver: State)(a: Constant[_], b: Constant[_]) =
        Truth(a.value == b.value)
    }

    val variableXlabel = labels.map(Piece(Variable, _, VarLeft))
    val freeLikeLabelXfreeLikeLabel = labels.collect({
      case l: FreeLabel0 => Piece(l, l, FreeNode0FreeNode0)
      case l: FreeLabel1 => Piece(l, l, FreeNode1FreeNode1)
      case l: FreeLabel2 => Piece(l, l, FreeNode2FreeNode2)
      case l: FreeLabel3 => Piece(l, l, FreeNode3FreeNode3)
      case l: FreeLabel4 => Piece(l, l, FreeNode4FreeNode4)
      case l: ConstantLabel[_] => Piece(l, l, Constants)
    })

    val assoc = labels.flatMap({
      case l: AssocLabel =>
        labels.collect({ case ll if !ll.isInstanceOf[Variable] => Piece(l, ll, AssocTerm) })
      case _ => Set[Piece]()
    }).toSet

    val anywhereContextMatchers = labels.map(Piece(AnywhereContext, _, new AnywhereContextMatcher()))

    new Apply(variableXlabel | freeLikeLabelXfreeLikeLabel | assoc | anywhereContextMatchers, env)
  }

}
