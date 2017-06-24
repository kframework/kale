package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply

import scala.collection.{+:, Iterable, Seq}

trait ACMixin extends kale.ACMixin with Environment with HasMatcher with HasUnifier {

  override def AssocWithIdLabel(name: String, id: Term): AssocWithIdLabel = new AssocWithIdListLabel(name, id)

  private object MatchesVar {
    def unapply(t: Term): Option[Term] = t match {
      case v: Variable => Some(t)
      case Rewrite(_: Variable, _) => Some(t)
      case _ => None
    }
  }

  private def matchContents(l: AssocLabel, soFar: Term, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: Apply): Term = {
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

  def AssocWithIdTerm(solver: Apply) = { (a: AssocWithIdList, b: Term) =>
    val asList = a.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(a.label, Next(a.label.identity), l1, l2)(solver)
  }

  case class TermAssocWithId(solver: Apply) extends Binary.F({ (a: Term, b: AssocWithIdList) =>
    val asList = b.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(b.label, Next(b.label.identity), l1, l2)(solver)
  })

  override protected def makeMatcher: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (_: AssocWithIdLabel, right) if !right.isInstanceOf[Variable] => AssocWithIdTerm
    case (left, _: AssocWithIdLabel) if !left.isInstanceOf[Variable] => TermAssocWithId
  }).orElse(super.makeMatcher)

  override def makeUnifier: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (_: AssocWithIdLabel, right) if !right.isInstanceOf[Variable] => AssocWithIdTerm
    case (left, _: AssocWithIdLabel) if !left.isInstanceOf[Variable] => TermAssocWithId
  }).orElse(super.makeUnifier)
}

private[standard] class AssocWithIdListLabel(val name: String, val identity: Term)(implicit val env: Environment) extends AssocWithIdLabel with Constructor {
  protected override def construct(l: Iterable[Term]): Term = AssocWithIdList(this, l)
}

private[standard] case class AssocWithIdList(label: AssocWithIdLabel, assocIterable: Iterable[Term]) extends Assoc {
  assert(assocIterable.size > 1)

  assert(assocIterable.forall(_ != label.identity))
  assert(assocIterable.forall(_.label != label))

  override lazy val isPredicate: Boolean = false

  override def _1: Term = assocIterable.head

  override def _2: Term = label(assocIterable.tail)
}
