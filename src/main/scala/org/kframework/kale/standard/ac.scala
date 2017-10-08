package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.Named

import scala.collection.{+:, Iterable, Seq}

trait NonAssocWithIdListMixin extends Mixin {
  _: Environment with FreeMixin =>

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

    override val isPredicate: Option[Boolean] = Some(false)
  }

  case class NonAssocWithIdTerm(solver: Apply) extends Binary.F({ (a: Node2, b: Term) =>
    val label = a.label.asInstanceOf[NonAssocWithIdLabel]
    val identity = label.identity

    val res = (a, b) match {
      case (label(a1, a2), label(b1, b2)) =>
        Or(List(
          FreeNode2FreeNode2(solver)(a, b)
        ))
      case (label(a1, a2), _) =>
        Or(List(
          And.combine(label)(Task(a1, b), Task(a2, identity))
        ))
    }
    res
  })

  //#KSequence(__(_=_;('n, 10), _=_;('sum, 0)), while(_)_(!_(_<=_('n, 0)), {_}(__(_=_;('sum, _+_('sum, 'n)), _=_;('n, _+_('n, -1))))))

  register(Binary.definePartialFunction({
    case (_: NonAssocWithIdLabel, right) if !right.isInstanceOf[Variable] => NonAssocWithIdTerm
  }), Priority.medium)
}

trait AssocWithIdListMixin extends Mixin {
  _: Environment with kale.ACMixin with IntMixin with MatchingLogicMixin =>

  override def AssocWithIdLabel(name: String, id: Term): NonPrimitiveMonoidLabel = new MonoidListLabel(name, id)

  private def matchContents(l: SemigroupLabel, soFar: Term, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: Apply): Term = strongBottomize(soFar) {
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
                case v: Variable => And.combine(l)(Solved(soFar), Solved(And(prefixTerm, Equality(v, prefixTerm))))
                case _ => And.combine(l)(Solved(soFar), Task(t, prefixTerm))
              }
              matchContents(l, newSoFar, tailL, suffix)
          }
          .fold(Bottom)({
            (a, b) => Or(a, b)
          })
      case (left, right) if left.nonEmpty && right.nonEmpty =>
        val And.SPN(sub, _, _) = soFar
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
    matchContents(a.label, a.label.identity, l1, l2)(solver)
  }

  case class TermAssocWithId(solver: Apply) extends Binary.F({ (a: Term, b: AssocWithIdList) =>
    val asList = b.label.asIterable _
    val l1 = asList(a)
    val l2 = asList(b)
    matchContents(b.label, b.label.identity, l1, l2)(solver)
  })

  register(Binary.definePartialFunction({
    case (_: MonoidLabel, right) if !right.isInstanceOf[Variable] => AssocWithIdTerm
  }), Priority.medium)
}

case class CollectionSize(collectionLabel: CollectionLabel)(implicit env: Environment with IntMixin) extends Named(collectionLabel.name + ".size") with FunctionLabel1 {
  override def f(_1: Term): Option[Term] =
    if (_1.isGround)
      Some(env.INT.Int(collectionLabel.asIterable(_1).size))
    else
      None

  /**
    * None means that it depends on its children
    */
  override val isPredicate: Option[Boolean] = Some(false)
}

private[standard] class MonoidListLabel(val name: String, val identity: Term)(implicit val env: Environment with IntMixin) extends NonPrimitiveMonoidLabel with Constructor {

  val size = CollectionSize(this)

  protected override def construct(l: Iterable[Term]): Term = AssocWithIdList(this, l)

  /**
    * None means that it depends on its children
    */
  override val isPredicate: Option[Boolean] = Some(false)
}

case class AssocWithIdList(label: MonoidLabel, assocIterable: Iterable[Term]) extends Assoc {
  assert(assocIterable.size > 1)

  assert(assocIterable.forall(_ != label.identity))
  assert(assocIterable.forall(_.label != label))

  override lazy val isPredicate: Boolean = false

  override def _1: Term = assocIterable.head

  override def _2: Term = label(assocIterable.tail)
}
