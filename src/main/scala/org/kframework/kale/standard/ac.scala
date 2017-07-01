package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.Named

import scala.collection.{+:, Iterable, Seq}

trait NonAssocWithIdListMixing extends Environment with FreeMixin with HasMatcher {

  case class NonAssocWithIdLabel(override val name: String, identity: Term) extends Named(name) with Label2 with HasId {
    val self = this
    override def apply(_1: Term, _2: Term): Term = (_1, _2) match {
      case (`identity`, b) => b
      case (a, `identity`) => a
      case (self(a, b), c) =>
        FreeNode2(this, a, FreeNode2(this, b, c))
      case (a, b) =>
        FreeNode2(this, a, b)
    }
  }

  case class NonAssocWithIdTerm(solver: Apply) extends Binary.F({ (a: Node2, b: Term) =>
    val label = a.label.asInstanceOf[NonAssocWithIdLabel]
    val identity = label.identity

    val res = (a, b) match {
      case (label(rw@Rewrite(label(_, _), _), a3), label(b1, label(b2, b3))) =>
        And.combine(label)(Task(rw, label(b1, b2)), Task(a3, b3))
      case (label(a1, a2), label(b1, b2)) =>
        Or(List(
          FreeNode2FreeNode2(solver)(a, b),
          And.combine(label)(Task(a1, b), Task(a2, identity)),
          And.combine(label)(Task(a1, identity), Task(a2, b))
        ))
      case (label(a1, a2), _) =>
        Or(List(
          And.combine(label)(Task(a1, b), Task(a2, identity)),
          And.combine(label)(Task(a1, identity), Task(a2, b))
        ))
    }
    res
  })

  //#KSequence(__(_=_;('n, 10), _=_;('sum, 0)), while(_)_(!_(_<=_('n, 0)), {_}(__(_=_;('sum, _+_('sum, 'n)), _=_;('n, _+_('n, -1))))))

  override protected def makeMatcher: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (_: NonAssocWithIdLabel, right) if !right.isInstanceOf[Variable] => NonAssocWithIdTerm
  }).orElse(super.makeMatcher)
}

trait AssocWithIdListMixin extends kale.ACMixin with Environment with HasMatcher with HasUnifier {

  override def AssocWithIdLabel(name: String, id: Term): AssocWithIdLabel = new AssocWithIdListLabel(name, id)

  private def matchContents(l: AssocLabel, soFar: Term, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: Apply): Term = strongBottomize(soFar) {


    val res = (ksLeft.toSeq, ksRight.toSeq) match {
      case (Seq(), Seq()) =>
        soFar
      case (t +: tailL, ksR) =>
        (0 to ksR.size)
          .map {
            index => (ksR.take(index), ksR.drop(index))
          }
          .map {
            case (prefix, suffix) =>
              val prefixTerm = l(prefix)
              val newSoFar = t match {
                case v: Variable => And.combine(l)(Solved(soFar), Solved(And(Next(prefixTerm), Equality(v, prefixTerm))))
                case _ => And.combine(l)(Solved(soFar), Task(t, prefixTerm))
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
  }).orElse(super.makeMatcher)

  override def makeUnifier: Binary.ProcessingFunctions = Binary.definePartialFunction({
    case (_: AssocWithIdLabel, right) if !right.isInstanceOf[Variable] => AssocWithIdTerm
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
