package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.context.anywhere.AnywhereContextMatcher
import org.kframework.kale.context.pattern.{PatternContextApplicationLabel, PatternContextMatcher}
import org.kframework.kale.transformer.Binary

import scala.collection.{+:, Iterable, Seq}

case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
  AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)

object SingleSortedMatcher {
  def apply()(implicit env: StandardEnvironment) = new SingleSortedMatcher()
}

class SingleSortedMatcher()(implicit val env: StandardEnvironment) extends MatcherOrUnifier {

  import Binary._
  import env._
  import org.kframework.kale.context._
  import org.kframework.kale.util.StaticImplicits._

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

  def AssocTerm(solver: Apply)(a: Assoc, b: Term) = {
    val asList = a.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(a.label, Top, l1, l2)(solver)
  }

  def TermAssoc(solver: Apply)(a: Term, b: Assoc) = {
    val asList = b.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(b.label, Top, l1, l2)(solver)
  }

  def MapTerm(solver: Apply)(a: Term, b: Term): Term = a.label match {
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

  // TODO: something is not quite right with FormulaLabel -- make sure it is correct
  def OneIsFormula(solver: Apply)(a: Term, b: Term) = And(a, b)

  def IfThenElseTerm(solver: Apply)(a: Node3, b: Term): Term = {
    val c = solver(a._1, b)
    if (c == Bottom)
      a._3
    else
      And(c, a._2)
  }

  def BindMatchMatcher(solver: Apply)(a: Node2, b: Term): Term = {
    val v = a._1.asInstanceOf[Variable]
    val p = a._2
    Or(Or.asSet(b) map { bx =>
      val sol = solver(p, bx)
      And(Equality(v, bx), sol)
    })
  }

  import standard._

  override def processingFunctions: ProcessingFunctions =
    definePartialFunction({
      case (`BindMatch`, _) => BindMatchMatcher _
      case (`IfThenElse`, _) => IfThenElseTerm _
      case (_, `Not`) => OneIsFormula _
      case (`Not`, _) => OneIsFormula _
      case (`And`, _) => AndTerm _
      case (_, `And`) => TermAnd _
      case (`Or`, _) => OrTerm _
      case (_, `Or`) => TermOr _
      case (`Variable`, _) => VarLeft _
      // case (_, `Variable`) => VarRight
      case (`AnywhereContext`, _) => new AnywhereContextMatcher()(env)
      case (capp: PatternContextApplicationLabel, _) => new PatternContextMatcher()(env)
    })
      .orElse(freeLabelProcessing)
      .orElse(functionDefinedByRewritingProcessing)
      .orElse(definePartialFunction({
        case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants _
        case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm _
        case (_: AssocLabel, right) if !right.isInstanceOf[Variable] => AssocTerm _
      }))
      .orElse(super.processingFunctions)
}
