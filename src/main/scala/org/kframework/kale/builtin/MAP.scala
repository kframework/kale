package org.kframework.kale.builtin

import org.kframework.kale._
import org.kframework.kale.standard.{MatchNotSupporteredError, Solved, Task}
import org.kframework.kale.transformer.Binary
import org.kframework.kale.transformer.Binary.{Apply, ProcessingFunctions}

import scala.collection.{Iterable, Map, Set}

trait MapMixin extends Mixin {
  _: Environment with standard.MatchingLogicMixin with HasMatcher =>

  register({
    case (_: MapLabel, right) if !right.isInstanceOf[Variable] => MapTerm
  }, Priority.medium)

  case class MapTerm(solver: Apply) extends Binary.F({ (a: Term, b: Term) =>
    a.label match {
      case mapLabel: MapLabel =>
        val mapLabel.indexedAndUnindexed(leftMap, leftUnindexed) = a
        val mapLabel.indexedAndUnindexed(rightMap, rightUnindexed) = b

        assert(leftMap.size + leftUnindexed.size > 1, "There is some bug in the Piece registration")


        if (rightUnindexed.nonEmpty) {
          throw MatchNotSupporteredError(a, b, "Unindexed on the rhs.")
        } else if (leftMap.nonEmpty && rightMap.isEmpty && rightUnindexed.isEmpty) {
          Bottom
        } else if (
          leftMap.isEmpty
            && leftUnindexed.exists({ case ForAll(v1, v2) => v1 == v2; case v: Variable => true; case _ => false })
            && leftUnindexed.exists({ case Rewrite(mapLabel.identity, _) => true; case _ => false })) {
          val leftVar = leftUnindexed.find({ case ForAll(v1, v2) => v1 == v2; case v: Variable => true; case _ => false }).get
          val rhs = leftUnindexed.collect({ case Rewrite(mapLabel.identity, r) => r }).head

          val nextTerm = if (rightMap.size + rightUnindexed.size == 0) {
            Next(rhs)
          } else {
            if (mapLabel.isIndexable(rhs)) {
              Next(MapImplementation(mapLabel, rightMap + (mapLabel.indexFunction(rhs) -> rhs), rightUnindexed))
            } else {
              Next(MapImplementation(mapLabel, rightMap, rightUnindexed + rhs))
            }
          }
          And(And.onlyPredicate(solver(leftVar, b)), nextTerm)

        } else if (leftMap.nonEmpty && rightMap.nonEmpty && leftUnindexed.size <= 1 && rightUnindexed.isEmpty) {
          val leftKeys = leftMap.keys.toSet
          val rightKeys = rightMap.keys.toSet


          if (!rightKeys.forall(_.isGround)) {
            throw MatchNotSupporteredError(a, b)
          }

          if (!(leftKeys filter (_.isGround) forall rightKeys.contains)) {
            Bottom
          } else if (leftKeys.size - (leftKeys & rightKeys).size <= 1) {

            val commonKeys = leftKeys & rightKeys
            import mapLabel._

            val valueMatchesTasks: Term = if (commonKeys.nonEmpty)
              And.combine(mapLabel)(commonKeys map (k => Task(leftMap(k), rightMap(k))) toSeq: _*)
            else
              identity

            val lookupByKeyVariableAndValueMatch = if (leftKeys.size - commonKeys.size == 1) {
              val v = (leftKeys -- rightKeys).head
              val rightValue = (rightKeys -- leftKeys).head

              And(Equality(v, rightValue), leftMap(v), rightMap(rightValue))
              ???
            } else {
              Top
            }

            val freeLeftVariableEqualityTask = if (leftUnindexed.size == 1) {
              val value = mapLabel((rightKeys -- leftKeys).map(rightMap))
              And(And.onlyPredicate(solver(leftUnindexed.head, value)), value)
            } else {
              mapLabel.identity
            }

            if (And.onlyPredicate(lookupByKeyVariableAndValueMatch) != Top && And.onlyPredicate(freeLeftVariableEqualityTask) != Top) {
              throw MatchNotSupporteredError(a, b)
            }

            And.combine(mapLabel)(Solved(valueMatchesTasks), Solved(freeLeftVariableEqualityTask)) //lookupByKeyVariableAndValueMatch
          } else {
            throw MatchNotSupporteredError(a, b, "Only supported matches with at most one differing (i.e., symbolic somehow) key and at most a variable (at the top level) on the rhs.")
          }
        } else {
          throw MatchNotSupporteredError(a, b, "Not yet implemented. Should eventually default to AC.")
        }
    }
  })

}

case class MapLabel(name: String, indexFunction: Term => Term, identity: Term)(implicit val env: Environment) extends NonPrimitiveMonoidLabel with Constructor {
  override val isPredicate: Option[Boolean] = Some(false)

  def isIndexable(t: Term) =
    !t.label.isInstanceOf[VariableLabel] &&
      !t.label.isInstanceOf[FunctionLabel] &&
      !t.label.isInstanceOf[RewriteLabel] &&
      !t.label.isInstanceOf[ForAllLabel]

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

  object indexedAndUnindexed {
    def unapply(m: Term): Option[(Map[Term, Term], Set[Term])] = m match {
      case m: MapImplementation if m.label == MapLabel.this => Some(m.map, m.unindexable)
      case `identity` => Some(Map[Term, Term](), Set[Term]())
      case t if isIndexable(t) => Some(Map(indexFunction(t) -> t), Set[Term]())
      case t if !isIndexable(t) => Some(Map[Term, Term](), Set(t))
    }
  }

  /**
    * returns the entire object that has been indexed
    */
  object lookupByKey extends {
    override val isPredicate: Option[Boolean] = Some(false)
    val name = MapLabel.this.name + ".lookupByKey"
  } with HasEnvironment with FunctionLabel2 {
    def f(m: Term, key: Term) = m match {
      case indexedAndUnindexed(scalaMap, restOfElements) =>
        scalaMap.get(key).orElse(
          if (restOfElements.isEmpty && key.isGround && scalaMap.keys.forall(_.isGround)) Some(env.Bottom) else None)
      case _ => None
    }
  }

  /**
    * the classic map lookup
    */
  object lookup extends {
    override val isPredicate: Option[Boolean] = Some(false)
    val name = MapLabel.this.name + ".lookup"
  } with HasEnvironment with FunctionLabel2 {
    def f(m: Term, key: Term) = m match {
      case indexedAndUnindexed(scalaMap, restOfElements) =>
        scalaMap.get(key).map(_.children.toList(1)).orElse(
          if (restOfElements.isEmpty && key.isGround && scalaMap.keys.forall(_.isGround)) Some(env.Bottom) else None)
      case _ => None
    }
  }

}

class KeysFunction(mapLabel: MapLabel, returnedSetLabel: SetLabel)(implicit val env: Environment) extends {
  override val isPredicate: Option[Boolean] = Some(false)
  val name = mapLabel.name + ".keys"
} with FunctionLabel1 {
  def f(m: Term) = m match {
    case mapLabel.indexedAndUnindexed(scalaMap, restOfElements) =>
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