package org.kframework.kale

import org.kframework.kale.transformer.Binary

import scala.collection._

case class Matcher(env: CurrentEnvironment) {

  import context._
  import Binary._

  implicit val envv = env

  import env._

  import StaticImplicits._

  def shortCircuitAnd(solver: State)(toEqual: (Term, Term)*): Term = {
    toEqual.foldLeft(Top: Term)({
      case (Bottom, _) => Bottom
      case (soFar, (l, r)) =>
        val results = Or.asSet(soFar) map {
          case soFarVariant@And.substitutionAndTerms(sub, _) =>
            And(soFarVariant: Term, solver(sub(l), sub(r)))
        }
        Or(results)
    })
  }

  object FreeNode0FreeNode0 extends ProcessingFunction[Node0, Node0, Top.type] {
    def f(solver: State)(a: Node0, b: Node0) = Top
  }

  object FreeNode1FreeNode1 extends ProcessingFunction[Node1, Node1, Term] {
    def f(solver: State)(a: Node1, b: Node1) = shortCircuitAnd(solver)((a._1, b._1))
  }

  object FreeNode2FreeNode2 extends ProcessingFunction[Node2, Node2, Term] {
    def f(solver: State)(a: Node2, b: Node2) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2))
  }

  object FreeNode3FreeNode3 extends ProcessingFunction[Node3, Node3, Term] {
    def f(solver: State)(a: Node3, b: Node3) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3))
  }

  object FreeNode4FreeNode4 extends ProcessingFunction[Node4, Node4, Term] {
    def f(solver: State)(a: Node4, b: Node4) = And(List(solver(a._1, b._1), solver(a._2, b._2), solver(a._3, b._3), solver(a._4, b._4)))
  }

  def matchContents(l: AssocLabel, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: State): Term = {
    val res = (ksLeft.toSeq, ksRight.toSeq) match {
      case (Seq(), Seq()) => Top
      case ((v: Variable) +: tailL, ksR) =>
        (0 to ksR.size)
          .map {
            index => (ksR.take(index), ksR.drop(index))
          }
          .map {
            case (prefix, suffix) => And(Equality(v, l(prefix)), matchContents(l, tailL, suffix))
          }
          .fold(Bottom)({
            (a, b) => Or(a, b)
          })
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

  case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
    AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)

  object MapTerm extends ProcessingFunction[Term, Term, Term] {
    override def f(solver: State)(a: Term, b: Term): Term = a.label match {
      case mapLabel: builtin.MapLabel =>
        val mapLabel.map(left, leftUnindexed) = a
        val mapLabel.map(right, rightUnindexed) = b

        assert(left.size + leftUnindexed.size > 1, "There is some bug in the Piece registration")

        if (rightUnindexed.nonEmpty) {
          throw MatchNotSupporteredError(a, b, "Var on the rhs.")
        }
        else if (left.nonEmpty && right.isEmpty && rightUnindexed.isEmpty) {
          Bottom
        }
        else if (left.nonEmpty && right.nonEmpty && leftUnindexed.size <= 1 && rightUnindexed.isEmpty) {
          val leftKeys = left.keys.toSet
          val rightKeys = right.keys.toSet


          if (!rightKeys.forall(_.isGround)) {
            throw MatchNotSupporteredError(a, b)
          }

          if (!(leftKeys filter (_.isGround) forall rightKeys.contains)) {
            Bottom
          }
          else if (leftKeys.size - (leftKeys & rightKeys).size <= 1) {

            val commonKeys = leftKeys & rightKeys

            val valueMatches = if (commonKeys.nonEmpty)
              And(commonKeys map (k => solver(left(k), right(k))))
            else
              Top

            val lookupByKeyVariableAndValueMatch = if (leftKeys.size - commonKeys.size == 1) {
              val v = (leftKeys -- rightKeys).head
              val rightValue = (rightKeys -- leftKeys).head

              And(Equality(v, rightValue), left(v), right(rightValue))
            } else {
              Top
            }

            val freeLeftVariableEquality = if (leftUnindexed.size == 1) {
              Equality(leftUnindexed.head, mapLabel((rightKeys -- leftKeys).map(right)))
            } else {
              Top
            }

            if (lookupByKeyVariableAndValueMatch != Top && freeLeftVariableEquality != Top) {
              throw MatchNotSupporteredError(a, b)
            }

            And(valueMatches, lookupByKeyVariableAndValueMatch, freeLeftVariableEquality)
          } else {
            throw MatchNotSupporteredError(a, b, "Only supported matches with at most one differing (i.e., symbolic somehow) key and at most a variable (at the top level) on the rhs.")
          }
        }
        else {
          throw MatchNotSupporteredError(a, b, "Not yet implemented. Should eventually default to AC.")
        }
    }
  }

  object VarLeft extends ProcessingFunction[Variable, Term, Term] {
    def f(solver: State)(a: Variable, b: Term) = Equality(a.asInstanceOf[Variable], b)
  }

  object Constants extends ProcessingFunction[Constant[_], Constant[_], Term] {
    override def f(solver: State)(a: Constant[_], b: Constant[_]) =
      Truth(a.value == b.value)
  }

  object AndTerm extends ProcessingFunction[And, Term, Term] {
    override def f(solver: State)(a: And, b: Term): Term = {
      val solution = solver(a.nonFormula.get, b)
      And(a.formulas, solution)
    }
  }

  def default = {
    val variableXlabel = labels.map(Piece(Variable, _, VarLeft))
    val andXlabel = labels.map(Piece(And, _, AndTerm))

    val freeLikeLabelXfreeLikeLabel = labels.collect({
      case l: FreeLabel0 => Piece(l, l, FreeNode0FreeNode0)
      case l: FunctionDefinedByRewritingLabel0 => Piece(l, l, FreeNode0FreeNode0)
      case l: FreeLabel1 => Piece(l, l, FreeNode1FreeNode1)
      case l: FunctionDefinedByRewritingLabel1 => Piece(l, l, FreeNode1FreeNode1)
      case l: FreeLabel2 => Piece(l, l, FreeNode2FreeNode2)
      case l: FunctionDefinedByRewritingLabel2 => Piece(l, l, FreeNode2FreeNode2)
      case l: FreeLabel3 => Piece(l, l, FreeNode3FreeNode3)
      case l: FunctionDefinedByRewritingLabel3 => Piece(l, l, FreeNode3FreeNode3)
      case l: FreeLabel4 => Piece(l, l, FreeNode4FreeNode4)
      case l: FunctionDefinedByRewritingLabel4 => Piece(l, l, FreeNode4FreeNode4)
      case l: ConstantLabel[_] => Piece(l, l, Constants)
    })

    val assoc = labels.flatMap({
      case m: env.builtin.MapLabel =>
        labels.collect({
          case ll if !ll.isInstanceOf[Variable] => Piece(m, ll, MapTerm)
        })
      case l: AssocLabel if l != And =>
        labels.collect({
          case ll if !ll.isInstanceOf[Variable] => Piece(l, ll, AssocTerm)
        })
      case _ => Set[Piece]()
    })

    val anywhereContextMatchers = labels.map(Piece(AnywhereContext, _, new AnywhereContextMatcher()))

    val contextMatchers = labels.map(Piece(CAPP, _, new PatternContextMatcher()))

    new Apply(variableXlabel | andXlabel | freeLikeLabelXfreeLikeLabel | assoc | anywhereContextMatchers | contextMatchers, env)
  }

}
