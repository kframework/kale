package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.MatchNotSupporteredError
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunctions, definePartialFunction}

import scala.collection.{Iterable, Map, Set}

trait MapMixin extends Environment with HasMatcher {
  override protected def makeMatcher: ProcessingFunctions = Binary.definePartialFunction({
    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
  }).orElse(super.makeMatcher)

  case class MapTerm(solver: Apply) extends Binary.F({ (a: Term, b: Term) =>
    a.label match {
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
  })

}

case class MapLabel(name: String, indexFunction: Term => Term, identity: Term)(implicit val env: Environment) extends AssocWithIdLabel {
  def isIndexable(t: Term) = !t.label.isInstanceOf[VariableLabel] && !t.isInstanceOf[FunctionLabel]

  trait HasEnvironment {
    val env = MapLabel.this.env
  }

  override def construct(l: Iterable[Term]): Term = {
    val indexed = l
      .collect {
        case t if isIndexable(t) => (indexFunction(t), t)
      }
      .toMap
    val unindexed = (l filterNot isIndexable).toSet
    new MapImplementation(this, indexed, unindexed)
  }


  def apply(map: collection.Map[Term, Term], unindexable: Set[Term]): Term = (map.size, unindexable.size) match {
    case (0, 0) => identity
    case (1, 0) => map.head._2
    case (0, 1) => unindexable.head
    case _ => new MapImplementation(this, map, unindexable)
  }

  object map {
    def unapply(m: Term): Option[(Map[Term, Term], Set[Term])] = m match {
      case m: MapImplementation if m.label == MapLabel.this => Some(m.map, m.unindexable)
      case `identity` => Some(Map[Term, Term](), Set[Term]())
      case t if isIndexable(t) => Some(Map(indexFunction(t) -> t), Set[Term]())
      case t if !isIndexable(t) => Some(Map[Term, Term](), Set(t))
    }
  }

  // returns the entire object that has been indexed
  object lookupByKey extends {
    val name = MapLabel.this.name + ".lookupByKey"
  } with HasEnvironment with FunctionLabel2 {
    def f(m: Term, key: Term) = m match {
      case map(scalaMap, restOfElements) =>
        scalaMap.get(key).orElse(
          if (restOfElements.isEmpty && key.isGround && scalaMap.keys.forall(_.isGround)) Some(env.Bottom) else None)
      case _ => None
    }
  }

  // the classic map lookup
  object lookup extends {
    val name = MapLabel.this.name + ".lookup"
  } with HasEnvironment with FunctionLabel2 {
    def f(m: Term, key: Term) = m match {
      case map(scalaMap, restOfElements) =>
        scalaMap.get(key).map(_.children.toList(1)).orElse(
          if (restOfElements.isEmpty && key.isGround && scalaMap.keys.forall(_.isGround)) Some(env.Bottom) else None)
      case _ => None
    }
  }

}

class KeysFunction(mapLabel: MapLabel, returnedSetLabel: SetLabel)(implicit val env: Environment) extends {
  val name = mapLabel.name + ".keys"
} with FunctionLabel1 {
  def f(m: Term) = m match {
    case mapLabel.map(scalaMap, restOfElements) =>
      Some(returnedSetLabel(scalaMap.keys))
    case _ => None
  }
}

case class MapImplementation(label: MapLabel, map: collection.Map[Term, Term], unindexable: Set[Term]) extends Assoc {
  lazy val assocIterable = unindexable ++ map.values

  override def _1: Term = unindexable.headOption.getOrElse(map.head._2)

  override lazy val isPredicate: Boolean = false

  override def _2: Term =
    if (unindexable.nonEmpty)
      label(map, unindexable.tail)
    else
      label(map.tail, unindexable)


  def equals(other: Term) = other match {
    case that: MapImplementation => this.label == that.label && this.map == that.map && this.unindexable == that.unindexable
    case _ => false
  }
}