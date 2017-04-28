package org.kframework.kale.builtin

import org.kframework.kale._

import scala.collection.{Iterable, Map, Set}


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