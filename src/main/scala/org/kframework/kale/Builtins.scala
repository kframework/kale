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
    override def construct(l: Iterable[Term]): Term = new MAP(this, l.map(t => (index(t), t)).toMap)

    def apply(map: collection.Map[Term, Term]): Term = map.size match {
      case 0 => identity
      case 1 => map.head._2
      case 2 => new MAP(this, map)
    }

    object map {
      def unapply(m: Term): Option[Map[Term, Term]] = m match {
        case m: MAP => Some(m.map)
        case `identity` => Some(Map[Term, Term]())
        case t => Some(Map(index(t) -> t))
      }
    }

  }

  object MAP {

    object remove extends NameFromObject with HasEnvironment with Label2 with Hooked {
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
    lazy val assocIterable = map map { case (_1, _2) => label.elementLabel(_1, _2) }

    override def _1: Term = label.elementLabel(map.head._1, map.head._2)

    override def _2: Term = label(map.tail)

    def equals(other: Term) = other match {
      case that: MAP => this.label == that.label && this.map == that.map
      case _ => false
    }
  }

  MAP

}