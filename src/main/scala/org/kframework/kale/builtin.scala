package org.kframework.kale

import collection._

object INT extends ConstantLabel[Int] {

  object + extends FreeLabel2(UniqueId(), "+")

}

object STRING extends ConstantLabel[String]
object DOUBLE extends ConstantLabel[Double]
object BOOLEAN extends ConstantLabel[Boolean]

object Tuple2 extends FreeLabel2(UniqueId(), "Pair")

case class MapLabel(name: String, identity: Term) extends AssocWithIdLabel with UniqueId {
  override def construct(l: Iterable[Term]): Term = ???

  def apply(map: collection.Map[Term, Term]): Term = map.size match {
    case 0 => identity
    case 1 => Tuple2(map.head._1, map.head._2)
    case 2 => new MAP(this, map)
  }

  object map {
    def unapply(m: Term): Option[Map[Term, Term]] = m match {
      case m: MAP => Some(m.map)
      case `identity` => Some(Map[Term, Term]())
      case Tuple2(_1, _2) => Some(Map(_1 -> _2))
      case _ => throw new AssertionError("Not a map")

    }
  }

}

object MAP {

  object remove extends Label2 with NameFromObject with UniqueId with Hooked {
    def apply(map: Term, key: Term): Term = FreeNode2(this, map, key)

    def f(t: Term) = t match {
      case remove(map, key) => map.label match {
        case l: MapLabel => map match {
          case l(m: Map[Term, Term]) => l(m - key)
        }
        case _ => throw new AssertionError("Trying to remove an element from a non-MAP")
      }
    }
  }

}

class MAP(val label: MapLabel, val map: collection.Map[Term, Term]) extends Assoc {
  lazy val assocIterable = map map { case (_1, _2) => Tuple2(_1, _2) }

  override def _1: Term = Tuple2(map.head._1, map.head._2)

  override def _2: Term = label(map.tail)

  def equals(other: Term) = other match {
    case that: MAP => this.label == that.label && this.map == that.map
    case _ => false
  }
}
