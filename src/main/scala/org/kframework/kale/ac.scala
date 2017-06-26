package org.kframework.kale

trait ACMixin extends Mixin {
  def AssocWithIdLabel(name: String, id: Term): AssocWithIdLabel
}

trait HasId {
  val identity: Term
}

trait AssocLabel extends Label2 {
  override def apply(l: Iterable[Term]): Term

  private val thisthis = this

  def asIterable(t: Term): Iterable[Term] = t.label match {
    case `thisthis` => t.asInstanceOf[Assoc].assocIterable
    case _ => List(t)
  }

  object iterable {
    def unapply(t: Term): Option[Iterable[Term]] = Some(asIterable(t))
  }

  def map(f: Term => Term): Term => Term = { t: Term =>
    env.strongBottomize(t) {
      this (asIterable(t) map f)
    }
  }
}

trait AssocWithIdLabel extends AssocLabel with HasId {

  // normalizing
  def apply(_1: Term, _2: Term): Term = {
    val l1 = asIterable(_1)
    val l2 = asIterable(_2)
    l1 ++ l2 filterNot (_ == identity) match {
      case l if l.isEmpty => identity
      case l if l.size == 1 => l.head
      case l => construct(l)
    }
  }

  val self = this

  override def asIterable(t: Term): Iterable[Term] = t match {
    case `identity` => List[Term]()
    case x if x.label == this => x.asInstanceOf[Assoc].assocIterable
    case y => List(y)
  }

  // normalizing
  override def apply(list: Iterable[Term]): Term = (list fold identity) ((a, b) => apply(a, b))

  // not normalizing
  protected def construct(l: Iterable[Term]): Term
}

trait AssocWithoutIdLabel extends AssocLabel {
  // todo
}

trait Assoc extends Node2 with BinaryInfix {
  override val label: AssocLabel
  val assocIterable: Iterable[Term]
}

trait Comm

trait AssocComm extends Assoc with Comm {
  override val label: AssocCommLabel

  def asSet: Set[Term]
}

trait CommLabel

trait AssocCommLabel extends AssocLabel with CommLabel {
  def asSet(t: Term): Set[Term] = t match {
    case t: AssocComm if t.label == this => t.asSet
    case _ => Set(t)
  }

  object set {
    def unapply(t: Term): Option[Set[Term]] = Some(asSet(t))
  }

}

trait AssocCommWithIdLabel extends AssocCommLabel with HasId {
  override def asSet(t: Term): Set[Term] =
    if (t == identity) {
      Set()
    } else {
      super.asSet(t)
    }
}