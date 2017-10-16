package org.kframework.kale.standard

import org.kframework.kale
import org.kframework.kale._
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.Apply
import org.kframework.kale.util.{LabelNamed, timer}
import org.roaringbitmap.RoaringBitmap

import scala.annotation.switch
import scala.collection.{+:, Iterable, Seq}

trait NonAssocWithIdListMixin extends Mixin {
  _: Environment with FreeMixin =>

  case class NonAssocWithIdLabel(override val name: String, identity: Term) extends LabelNamed(name) with Label2 with HasId with Constructor {
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

  private def matchContents(l: SemigroupLabel, soFar: Term, ksLeft: List[Term], ksRight: List[Term])(implicit solver: Apply): Term =
    strongBottomize(soFar) {
      (ksLeft.size: @switch) match {
        case 0 =>
          if (ksRight.isEmpty)
            soFar
          else
            Bottom

        //        case 1 =>
        //          val t = ksLeft.head
        //          And.combine(l)(Solved(soFar), Task(t, l(ksRight)))

        case _ =>
          // assumes no variables on the RHS
          val t = ksLeft.head
          if (cannotMatchAssoc(l, t)) {
            if (ksRight.isEmpty) {
              Bottom
            } else {
              val newSoFar = And.combine(l)(Solved(soFar), Task(t, ksRight.head))
              matchContents(l, newSoFar, ksLeft.tail, ksRight.tail)
            }
          } else if (ksRight.nonEmpty && cannotMatchAssoc(l, ksLeft.last)) {
            And.combine(l)(
              Solved(matchContents(l, soFar, ksLeft.dropRight(1), ksRight.dropRight(1))),
              Task(ksLeft.last, ksRight.last))
          } else
            Or((0 to ksRight.size)
              .map { index: Int =>
                val prefix = ksRight.take(index)
                val suffix = ksRight.drop(index)
                val prefixTerm = l(prefix)
                val newSoFar = if (t.label == Variable) {
                  And.combine(l)(Solved(soFar), Solved(And(prefixTerm, Equality(t, prefixTerm))))
                } else {
                  And.combine(l)(Solved(soFar), Task(t, prefixTerm))
                }
                matchContents(l, newSoFar, ksLeft.tail, suffix)
              })
      }
    }


  private final def cannotMatchAssoc(l: SemigroupLabel, t: Term) = {
    Or.asSet(t).forall({
      case And.SPN(_, _, nonPredicate) =>
        nonPredicate.label != l && nonPredicate.label.isInstanceOf[Constructor]
    })
  }

  def AssocWithIdTerm(solver: Apply) = {
    (a: AssocWithIdList, b: Term) =>
      val asList = a.label.asIterable _
      val l1 = asList(a).asInstanceOf[List[Term]]
      val l2 = asList(b).asInstanceOf[List[Term]]
      matchContents(a.label, a.label.identity, l1, l2)(solver)
  }

  case class TermAssocWithId(solver: Apply) extends Binary.F({ (a: Term, b: AssocWithIdList) =>
    val asList = b.label.asIterable _
    val l1 = asList(a).asInstanceOf[List[Term]]
    val l2 = asList(b).asInstanceOf[List[Term]]
    matchContents(b.label, b.label.identity, l1, l2)(solver)
  })

  register(Binary.definePartialFunction({
    case (_: MonoidLabel, right) if !right.isInstanceOf[Variable] => AssocWithIdTerm
  }), Priority.medium)
}

case class CollectionSize(collectionLabel: CollectionLabel)(implicit env: Environment with IntMixin) extends LabelNamed(collectionLabel.name + ".size") with FunctionLabel1 {
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

  override def requiredLabels(children: Iterable[Term]): RoaringBitmap = {
    val res = Roaring.requiredFor(children)
    res.remove(empty.label.id)
    res
  }

  override def suppliedLabels(children: Iterable[Term]): RoaringBitmap = {
    val res = addThis(Roaring.suppliedBy(children))
    res.add(empty.label.id)
    res
  }

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
