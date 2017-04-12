package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.transformer.Binary

import scala.collection.{+:, Iterable, Seq}

case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
  AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)

case class SingleSortedMatcher()(implicit val env: CurrentEnvironment) extends MatcherOrUnifier {

  import Binary._
  import env._
  import org.kframework.kale.context._
  import org.kframework.kale.util.StaticImplicits._

  def shortCircuitAnd(solver: Apply)(toEqual: (Term, Term)*): Term = {
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

  object FreeNode0FreeNode0 extends ProcessingFunction[Apply] with TypedWith[Node0, Node0] {
    def f(solver: Apply)(a: Node0, b: Node0) = Top
  }

  object FreeNode1FreeNode1 extends ProcessingFunction[Apply] with TypedWith[Node1, Node1] {
    def f(solver: Apply)(a: Node1, b: Node1) = shortCircuitAnd(solver)((a._1, b._1))
  }

  object FreeNode2FreeNode2 extends ProcessingFunction[Apply] with TypedWith[Node2, Node2] {
    def f(solver: Apply)(a: Node2, b: Node2) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2))
  }

  object FreeNode3FreeNode3 extends ProcessingFunction[Apply] with TypedWith[Node3, Node3] {
    def f(solver: Apply)(a: Node3, b: Node3) = shortCircuitAnd(solver)((a._1, b._1), (a._2, b._2), (a._3, b._3))
  }

  object FreeNode4FreeNode4 extends ProcessingFunction[Apply] with TypedWith[Node4, Node4] {
    def f(solver: Apply)(a: Node4, b: Node4) = And(List(solver(a._1, b._1), solver(a._2, b._2), solver(a._3, b._3), solver(a._4, b._4)))
  }

  def matchContents(l: AssocLabel, soFar: Term, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: Apply): Term = {
    val res = (ksLeft.toSeq, ksRight.toSeq) match {
      case (Seq(), Seq()) =>
        soFar
      case ((v: Variable) +: tailL, ksR) =>
        (0 to ksR.size)
          .map {
            index => (ksR.take(index), ksR.drop(index))
          }
          .map {
            case (prefix, suffix) =>
              val bind = And(soFar, Equality(v, l(prefix)))
              matchContents(l, bind, tailL, suffix)
          }
          .fold(Bottom)({
            (a, b) => Or(a, b)
          })
      case (left, right) if left.nonEmpty && right.nonEmpty =>
        val headSolution: Term = solver(And(soFar, left.head), right.head)
        matchContents(l, headSolution, left.tail, right.tail)
      case other => Bottom
    }
    res
  }

  object AssocTerm extends ProcessingFunction[Apply] with TypedWith[Assoc, Term] {
    def f(solver: Apply)(a: Assoc, b: Term) = {
      val asList = a.label.asList _
      val l1 = asList(a)
      val l2 = asList(b)
      matchContents(a.label, Top, l1, l2)(solver)
    }
  }

  object TermAssoc extends ProcessingFunction[Apply] with TypedWith[Term, Assoc] {
    def f(solver: Apply)(a: Term, b: Assoc) = {
      val asList = b.label.asList _
      val l1 = asList(a)
      val l2 = asList(b)
      matchContents(b.label, Top, l1, l2)(solver)
    }
  }


  object MapTerm extends ProcessingFunction[Apply] with TypedWith[Term, Term] {
    override def f(solver: Apply)(a: Term, b: Term): Term = a.label match {
      case mapLabel: MapLabel =>
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

  object VarLeft extends ProcessingFunction[Apply] with TypedWith[Variable, Term] {
    def f(solver: Apply)(a: Variable, b: Term) = Equality(a.asInstanceOf[Variable], b)
  }

  object Constants extends ProcessingFunction[Apply] with TypedWith[Constant[_], Constant[_]] {
    override def f(solver: Apply)(a: Constant[_], b: Constant[_]) =
      Truth(a.value == b.value)
  }

  object AndTerm extends ProcessingFunction[Apply] with TypedWith[And, Term] {
    override def f(solver: Apply)(a: And, b: Term): Term = {
      val solution = solver(a.nonFormula.get, b)
      And(a.formulas, solution)
    }
  }

  object TermAnd extends ProcessingFunction[Apply] with TypedWith[Term, And] {
    override def f(solver: Apply)(a: Term, b: And): Term = {
      val solution = solver(a, b.nonFormula.get)
      And(solution, b.formulas)
    }
  }

  object OrTerm extends ProcessingFunction[Apply] with TypedWith[Or, Term] {
    def f(solver: Apply)(a: Or, b: Term) = {
      val sol = a.asSet map (solver(_, b))
      Or(sol)
    }
  }

  object TermOr extends ProcessingFunction[Apply] with TypedWith[Term, Or] {
    def f(solver: Apply)(a: Term, b: Or) = {
      val sol = b.asSet map (solver(a, _))
      Or(sol)
    }
  }

  // TODO: something is not quite right with FormulaLabel -- make sure it is correct
  object OneIsFormula extends ProcessingFunction[Apply] with TypedWith[Term, Term] {
    def f(solver: Apply)(a: Term, b: Term) = And(a, b)
  }

  object NoMatch extends ProcessingFunction[Apply] with TypedWith[Term, Term] {
    override def f(solver: Apply)(a: Term, b: Term): Term = Bottom
  }

  object IfThenElseTerm extends ProcessingFunction[Apply] with TypedWith[Node3, Term] {
    override def f(solver: Apply)(a: Node3, b: Term): Term = {
      val c = solver(a._1, b)
      if (c == Bottom)
        a._3
      else
        And(c, a._2)
    }
  }

  object BindMatchMatcher extends ProcessingFunction[Apply] with TypedWith[Node2, Term] {
    override def f(solver: Apply)(a: Node2, b: Term): Term = {
      val v = a._1.asInstanceOf[Variable]
      val p = a._2
      Or(Or.asSet(b) map { bx =>
        val sol = solver(p, bx)
        And(Equality(v, bx), sol)
      })
    }
  }

  import standard._

  override def processingFunctions: ProcessingFunctions = definePartialFunction({
    case (`BindMatch`, _) => BindMatchMatcher
    case (`IfThenElse`, _) => IfThenElseTerm
    case (_, `Not`) => OneIsFormula
    case (`Not`, _) => OneIsFormula
    case (`And`, _) => AndTerm
    case (_, `And`) => TermAnd
    case (`Or`, _) => OrTerm
    case (_, `Or`) => TermOr
    case (`Variable`, _) => VarLeft
    case (`AnywhereContext`, _) => new AnywhereContextMatcher()(env)
    case (capp: PatternContextApplicationLabel, _) => new PatternContextMatcher()(env)
    case (l1: FreeLabel, l2: FreeLabel) if l1 != l2 => NoMatch
    case (_: FreeLabel0, _: FreeLabel0) => FreeNode0FreeNode0
    case (_: FreeLabel1, _: FreeLabel1) => FreeNode1FreeNode1
    case (_: FreeLabel2, _: FreeLabel2) => FreeNode2FreeNode2
    case (_: FreeLabel3, _: FreeLabel3) => FreeNode3FreeNode3
    case (_: FreeLabel4, _: FreeLabel4) => FreeNode4FreeNode4
    case (_: FunctionDefinedByRewritingLabel0, _: FunctionDefinedByRewritingLabel0) => FreeNode0FreeNode0
    case (_: FunctionDefinedByRewritingLabel1, _: FunctionDefinedByRewritingLabel1) => FreeNode1FreeNode1
    case (_: FunctionDefinedByRewritingLabel2, _: FunctionDefinedByRewritingLabel2) => FreeNode2FreeNode2
    case (_: FunctionDefinedByRewritingLabel3, _: FunctionDefinedByRewritingLabel3) => FreeNode3FreeNode3
    case (_: FunctionDefinedByRewritingLabel4, _: FunctionDefinedByRewritingLabel4) => FreeNode4FreeNode4
    case (_: ConstantLabel[_], _: ConstantLabel[_]) => Constants
    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
    case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm
  }) orElse super.processingFunctions
}
