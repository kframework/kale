package org.kframework.kale.builtin

import org.kframework.kale._

import scala.collection.{Iterable, Set}

case class SetLabel(name: String, identity: Term)(implicit val env: Environment with HasBOOLEAN) extends AssocWithIdLabel {
  override def construct(l: Iterable[Term]): Term = SET(this, l.toSet)

  trait HasEnvironment {
    val env = SetLabel.this.env
  }

  object set {
    def unapply(t: Term): Some[Set[Term]] = t match {
      case `identity` => Some(Set())
      case SET(label, elements) if label == SetLabel.this => Some(elements)
      case t => Some(Set(t))
    }
  }

  object in extends {
    val name = SetLabel.this.name + ".in"
  } with HasEnvironment with FunctionLabel2 {
    def f(s: Term, key: Term) = s match {
      case set(elements) => Some(env.BOOLEAN(elements.contains(key)))
    }
  }

}


case class SET(label: SetLabel, elements: Set[Term]) extends Assoc {
  val assocIterable: Iterable[Term] = elements

  override def _1: Term = elements.head

  override def _2: Term = SET(label, elements.tail)
}