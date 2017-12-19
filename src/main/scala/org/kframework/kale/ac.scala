package org.kframework.kale

trait ACMixin extends Mixin {
  _: Environment =>
  def AssocWithIdLabel(name: String, id: Term): NonPrimitiveMonoidLabel
}

trait HasId {
  val identity: Term
  lazy val empty = identity
}

trait CollectionLabel extends Label2 {
  def asIterable(t: Term): Iterable[Term]

  object iterable {
    def unapply(t: Term): Option[Iterable[Term]] = Some(asIterable(t))
  }

  def map(f: Term => Term): Term => Term = { t: Term =>
    env.strongBottomize(t) {
      this (asIterable(t) map f)
    }
  }

  def filter(f: Term => Boolean): Term => Term = { t: Term =>
    env.strongBottomize(t) {
      this (asIterable(t) filter f)
    }
  }
}

trait SemigroupLabel extends CollectionLabel with cats.Semigroup[Term] {
  override def apply(list: Iterable[Term]): Term = list reduce apply

  private val thisthis = this

  def combine(a: Term, b: Term) = apply(a, b)

  def asIterable(t: Term): Iterable[Term] = t.label match {
    case `thisthis` => t match {
      case t: Assoc => t.assocIterable
      case _ => throw new AssertionError(t + " is not Assoc")
    }
    case _ => List(t)
  }
}

trait MonoidLabel extends SemigroupLabel with HasId with cats.Monoid[Term] {
  @Normalizing
  override def apply(list: Iterable[Term]): Term = (list fold identity) (apply)

  override def asIterable(t: Term): Iterable[Term] = t match {
    case `identity` => List[Term]()
    case x if x.label == this => x.asInstanceOf[Assoc].assocIterable
    case y => List(y)
  }
}

trait NonPrimitiveMonoidLabel extends MonoidLabel {

  @Normalizing
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

  @NonNormalizing
  protected def construct(l: Iterable[Term]): Term
}

trait AssocWithoutIdLabel extends SemigroupLabel {
  // todo
}

trait Assoc extends Node2 {
  override val label: SemigroupLabel
  val assocIterable: Iterable[Term]

  override def map0(f: (Term) => Term): Term = label(assocIterable map f)

  override def equals(obj: Any): Boolean = obj match {
    case that: Assoc => that.label == this.label && that.assocIterable == this.assocIterable
    case _ => false
  }

  override def toString: String = label.name + "(" + assocIterable.map(_.toString).mkString(", ") + ")"
}

object Assoc {
  def unapply(t: Term): Option[(SemigroupLabel, Iterable[Term])] = t.label match {
    case l: SemigroupLabel => Some(l, l.asIterable(t))
    case _ => None
  }
}

trait Comm

trait AssocComm extends Assoc with Comm {
  override val label: AssocCommLabel

  def asSet: Set[Term]

  override val assocIterable: Set[Term] = asSet

  override def toString: String = label.name + "(" + assocIterable.toList.map(_.toString).sorted.mkString(", ") + ")"
}

trait CommLabel

trait AssocCommLabel extends SemigroupLabel with CommLabel {
  def asSet(t: Term): Set[Term] = t match {
    case t: AssocComm if t.label == this => t.asSet
    case _ => Set(t)
  }

  object set {
    def unapply(t: Term): Option[Set[Term]] = Some(asSet(t))
  }

}

trait CommutativeMonoid extends AssocCommLabel with MonoidLabel {
  override final def asSet(t: Term): Set[Term] =
    if (t == identity) {
      Set()
    } else {
      super.asSet(t)
    }
}