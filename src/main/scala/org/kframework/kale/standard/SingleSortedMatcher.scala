package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.context.anywhere.AnywhereContextMatcher
import org.kframework.kale.context.pattern.{PatternContextApplicationLabel, PatternContextMatcher}
import org.kframework.kale.pretty.PrettyWrapperHolder
import org.kframework.kale.transformer.Binary

case class MatchNotSupporteredError(l: Term, r: Term, message: String = "") extends
  AssertionError("Trying to match " + l + " with " + r + " not supported yet. " + message)

object SingleSortedMatcher {
  def apply()(implicit env: StandardEnvironment) = new SingleSortedMatcher(env.makeMatcher)
}

class SingleSortedMatcher(input:  Binary.ProcessingFunctions)(implicit val env: StandardEnvironment) extends MatcherOrUnifier {

  import Binary._
  import env._

  override def apply(left: Term, right: Term): Term = {
    val res = super.apply(left, right)
//    assert(env.Or.asSet(res).forall({
//      case env.And.withNext(_, Some(_)) => true
//    }))
    res
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
    b.asOr map { bx =>
      val sol = solver(p, bx)
      And(Equality(v, bx), sol)
    }
  }

  def RewriteMatcher(solver: SingleSortedMatcher)(a: SimpleRewrite, b: Term): Term = {
    val env = solver.env
    import env._
    val m = solver(a._1, b)
    m.asOr map {
      case And.withNext(nonNext@And.substitutionAndTerms(subs, terms), _) =>
        val s = substitutionMaker(subs)
        And(Next(s(a._2)), nonNext)
    }
  }

  import standard._

  def TermPrettyWrapper(solver: Apply)(t: Term, a: PrettyWrapperHolder) = {
    t match {
      case v: Variable if v.sort == Sort("WhiteSpace") => VarLeft(solver)(v, a)
      case _ =>
        solver(t, a.content).asOr map {
          case And.withNext(p, Some(Next(n))) => And.withNext(p, Next(a.copy(a._1, n, a._3)))
          case Bottom => Bottom
        }
    }

  }

  def PrettyWrapperTerm(solver: Apply)(a: PrettyWrapperHolder, t: Term) = {
    if (t.isGround)
      Bottom
    else
      And.withNext(Equality(a, t), Next(a))
  }

  def PrettyWrapperPrettyWrapper(solver: Apply)(a: PrettyWrapperHolder, b: PrettyWrapperHolder) = {
    FreeNode3FreeNode3(solver)(a, b)
  }

  val strategyProcessing = {
    import STRATEGY._
    definePartialFunction({
      case (`orElse`, _) => strategy.orElseTerm
      case (`compose`, _) => strategy.composeTerm
      case (`repeat`, _) => strategy.repeatTerm
      case (`fixpoint`, _) => strategy.fixpointTerm
    })
  }

  override def processingFunctions: ProcessingFunctions =
    strategyProcessing.orElse(
      definePartialFunction({
        case (`Rewrite`, _) => RewriteMatcher
        case (PrettyWrapper, PrettyWrapper) => PrettyWrapperPrettyWrapper
        case (PrettyWrapper, term) => PrettyWrapperTerm
        case (`BindMatch`, _) => BindMatchMatcher
        case (`AnywhereContext`, _) => new AnywhereContextMatcher()(env)
        case (capp: PatternContextApplicationLabel, _) => new PatternContextMatcher()(env)
      }))
      .orElse(freeLabelProcessing)
      .orElse(functionDefinedByRewritingProcessing)
      .orElse(definePartialFunction({
        case (term, PrettyWrapper) => TermPrettyWrapper _
        case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants _
        case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm _
      }))
      .orElse(input)
}
