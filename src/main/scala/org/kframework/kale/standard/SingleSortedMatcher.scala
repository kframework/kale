package org.kframework.kale.standard

import org.kframework.kale._
import org.kframework.kale.builtin.MapLabel
import org.kframework.kale.context.anywhere.AnywhereContextMatcher
import org.kframework.kale.context.pattern.{PatternContextApplicationLabel, PatternContextMatcher}
import org.kframework.kale.pretty.PrettyWrapperHolder
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

  override def apply(left: Term, right: Term): Term = {
    val res = super.apply(left, right)
    assert(env.Or.asSet(res).forall({
      case env.And.withNext(_, Some(_)) => true
    }))
    res
  }

  object MatchesVar {
    def unapply(t: Term): Option[Term] = t match {
      case v: Variable => Some(t)
      case Rewrite(_: Variable, _) => Some(t)
      case _ => None
    }
  }

  def matchContents(l: AssocLabel, soFar: Term, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: Apply): Term = {
    val res = (ksLeft.toSeq, ksRight.toSeq) match {
      case (Seq(), Seq()) =>
        soFar
      case (MatchesVar(t) +: tailL, ksR) =>
        (0 to ksR.size)
          .map {
            index => (ksR.take(index), ksR.drop(index))
          }
          .map {
            case (prefix, suffix) =>
              val prefixTerm = l(prefix)
              val newSoFar = t match {
                case v: Variable => And.combine(l)(Solved(soFar), Solved(And(Next(prefixTerm), Equality(v, prefixTerm))))
                case Rewrite(v, right) => And.combine(l)(Solved(soFar), Solved(And(Next(right), Equality(v, prefixTerm))))
              }

              matchContents(l, newSoFar, tailL, suffix)
          }
          .fold(Bottom)({
            (a, b) => Or(a, b)
          })
      case (left, right) if left.nonEmpty && right.nonEmpty =>
        val (sub, _) = And.asSubstitutionAndTerms(soFar)
        val headSolution: Term = And.combine(l)(Solved(soFar), Task(sub(left.head), sub(right.head)))
        matchContents(l, headSolution, left.tail, right.tail)
      case _ => Bottom
    }
    res
  }

  def AssocWithIdTerm(solver: Apply)(a: AssocWithIdList, b: Term) = {
    val asList = a.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(a.label, Next(a.label.identity), l1, l2)(solver)
  }

  def TermAssocWithId(solver: Apply)(a: Term, b: AssocWithIdList) = {
    val asList = b.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(b.label, Next(b.label.identity), l1, l2)(solver)
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

  def RewriteMatcher(solver: SingleSortedMatcher)(a: SimpleRewrite, b: Term): Term = {
    val env = solver.env
    import env._
    val m = solver(a._1, b)
    Or(Or.asSet(m) map {
      case And.withNext(nonNext@And.substitutionAndTerms(subs, terms), _) =>
        val s = substitutionMaker(subs)
        And(Next(s(a._2)), nonNext)
    })
  }

  import standard._

  def TermPrettyWrapper(solver: Apply)(t: Term, a: PrettyWrapperHolder) = {
    Or(Or.asSet(solver(t, a.content)) map {
      case And.withNext(p, Some(Next(n))) => And.withNext(p, Next(a.copy(a._1, n , a._3)))
      case Bottom => Bottom
    })
  }

  def PrettyWrapperTerm(solver: Apply)(a: PrettyWrapperHolder, t: Term) = {
    Bottom
  }

  def PrettyWrapperPrettyWrapper(solver: Apply)(a: PrettyWrapperHolder, b: PrettyWrapperHolder) = {
    FreeNode3FreeNode3(solver)(a, b)
  }

  override def processingFunctions: ProcessingFunctions =
    definePartialFunction({
      case (PrettyWrapper, PrettyWrapper) => PrettyWrapperPrettyWrapper _
      case (term, PrettyWrapper) => TermPrettyWrapper _
      case (PrettyWrapper, term) => PrettyWrapperTerm _
      case (`Rewrite`, _) => RewriteMatcher _
      case (`BindMatch`, _) => BindMatchMatcher _
      case (_, `Not`) => OneIsFormula _
      case (`Not`, _) => OneIsFormula _
      case (`And`, _) => AndTerm _
      case (_, `And`) => TermAnd _
      case (`Or`, _) => OrTerm _
      case (_, `Or`) => TermOr _
      case (`Variable`, _) => VarLeft _
      case (_, `Variable`) => VarRight _
      case (`AnywhereContext`, _) => new AnywhereContextMatcher()(env)
      case (capp: PatternContextApplicationLabel, _) => new PatternContextMatcher()(env)
    })
      .orElse(freeLabelProcessing)
      .orElse(functionDefinedByRewritingProcessing)
      .orElse(definePartialFunction({
        case (_: DomainValueLabel[_], _: DomainValueLabel[_]) => Constants _
        case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm _
        case (_: AssocWithIdLabel, right) if !right.isInstanceOf[Variable] => AssocWithIdTerm _
        case (left, _: AssocWithIdLabel) if !left.isInstanceOf[Variable] => TermAssocWithId _
      }))
      .orElse(super.processingFunctions)
}
