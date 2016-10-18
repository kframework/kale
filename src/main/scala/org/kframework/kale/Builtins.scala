package org.kframework.kale

import collection._

class Builtins(implicit val env: Environment) {

  val eenv = env

  trait HasEnvironment {
    val env = eenv
  }

  sealed trait PrimordialConstantLabel[T] extends HasEnvironment with ConstantLabel[T]

  case class ReferenceLabel[T](name: String) extends HasEnvironment with PrimordialConstantLabel[T]

  val allPrimitives = List(INT, STRING, DOUBLE, BOOLEAN)

  // just for forcing initialization of "object" references

  object INT extends ReferenceLabel[Int]("Int")

  val `<=Int` = PrimitiveFunction2("_<=Int_", INT, BOOLEAN, (a: Int, b: Int) => a <= b)(env)
  val `+Int` = PrimitiveFunction2("_+Int_", INT, (a: Int, b: Int) => a + b)(env)

  object STRING extends ReferenceLabel[String]("String")

  object DOUBLE extends ReferenceLabel[Double]("Double")

  object BOOLEAN extends ReferenceLabel[Boolean]("Boolean")

  case class Sort(name: String)

  case class GENERIC_TOKEN(sort: Sort) extends {
    val name = "TOKEN_" + sort.name
  } with PrimordialConstantLabel[String]

  case class SetLabel(name: String, identity: Term)(implicit val env: Environment) extends AssocWithIdLabel {
    override def construct(l: Iterable[Term]): Term = SET(this, l.toSet)

    object set {
      def unapply(t: Term): Some[Set[Term]] = t match {
        case `identity` => Some(Set())
        case SET(label, elements) if label == SetLabel.this => Some(elements)
        case t => Some(Set(t))
      }
    }

    object in extends {
      val name = SetLabel.this.name + ".in"
    } with HasEnvironment with PurelyFunctionalLabel2 {
      def f(s: Term, key: Term) = s match {
        case set(elements) => Some(BOOLEAN(elements.contains(key)))
      }
    }

  }


  val BuiltinSetUnit = FreeLabel0(".Set")
  val BuiltinSet = SetLabel("_Set_", BuiltinSetUnit())

  case class SET(label: SetLabel, elements: Set[Term]) extends Assoc {
    val assocIterable: Iterable[Term] = elements
    override def _1: Term = elements.head
    override def _2: Term = SET(label, elements.tail)
  }

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
        case m: MAP if m.label == MapLabel.this => Some(m.map, m.unindexable)
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

    object keys extends {
      val name = MapLabel.this.name + ".keys"
    } with HasEnvironment with PurelyFunctionalLabel1 {
      def f(m: Term) = m match {
        case map(scalaMap, restOfElements) =>
          Some(BuiltinSet(scalaMap.keys))
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