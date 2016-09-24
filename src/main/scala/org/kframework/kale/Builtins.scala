package org.kframework.kale

import collection._

class Builtins(implicit val env: Environment) {

  val eenv = env

  trait HasEnvironment {
    val env = eenv
  }

  sealed trait PrimordialConstantLabel[T] extends HasEnvironment with ConstantLabel[T]

  object INT extends NameFromObject with PrimordialConstantLabel[Int] {

    object + extends FreeLabel2("+")(env)

  }

  INT

  object STRING extends NameFromObject with PrimordialConstantLabel[String]

  STRING

  object DOUBLE extends NameFromObject with PrimordialConstantLabel[Double]

  DOUBLE

  object BOOLEAN extends NameFromObject with PrimordialConstantLabel[Boolean]

  BOOLEAN

  case class Sort(s: String)

  case class GENERIC_TOKEN(sort: Sort) extends {
    val name = "TOKEN_" + sort.s
  } with PrimordialConstantLabel[String]

  case class MapLabel(name: String, index: Term => Term, identity: Term)(implicit val env: Environment) extends AssocWithIdLabel {
    def isIndexable(t: Term) = !t.label.isInstanceOf[VariableLabel] && !t.isInstanceOf[FunctionLabel]

    override def construct(l: Iterable[Term]): Term = {
      val indexed = l collect {
        case t if isIndexable(t) => (index(t), t)
      } toMap
      val unindexed = (l filterNot isIndexable).toSet
      new MAP(this, indexed, unindexed)
    }


    def apply(map: collection.Map[Term, Term], unindexable: Set[Term]): Term = (map.size, unindexable.size) match {
      case (0, 0) => identity
      case (1, 0) => map.head._2
      case (0, 1) => unindexable.head
      case _ => new MAP(this, map, unindexable)
    }

    object map {
      def unapply(m: Term): Option[(Map[Term, Term], Set[Term])] = m match {
        case m: MAP => Some(m.map, m.unindexable)
        case `identity` => Some(Map[Term, Term](), Set[Term]())
        case t if isIndexable(t) => Some(Map(index(t) -> t), Set[Term]())
        case t if !isIndexable(t) => Some(Map[Term, Term](), Set(t))
      }
    }

    // returns the entire object that has been indexed
    object lookupByKey extends {
      val name = MapLabel.this.name + ".lookupByKey"
    } with HasEnvironment with PurelyFunctionalLabel2 {
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
    } with HasEnvironment with PurelyFunctionalLabel2 {
      def f(m: Term, key: Term) = m match {
        case map(scalaMap, restOfElements) =>
          scalaMap.get(key).map(_.iterator().toList(1)).orElse(
            if (restOfElements.isEmpty && key.isGround && scalaMap.keys.forall(_.isGround)) Some(env.Bottom) else None)
        case _ => None
      }
    }

  }

  class MAP(val label: MapLabel, val map: collection.Map[Term, Term], val unindexable: Set[Term]) extends Assoc {
    lazy val assocIterable = unindexable ++ map.values

    override def _1: Term = unindexable.headOption.getOrElse(map.head._2)

    override def _2: Term =
      if (unindexable.nonEmpty)
        label(map, unindexable.tail)
      else
        label(map.tail, unindexable)


    def equals(other: Term) = other match {
      case that: MAP => this.label == that.label && this.map == that.map && this.unindexable == that.unindexable
      case _ => false
    }
  }

}