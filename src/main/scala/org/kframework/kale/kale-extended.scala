package org.kframework.kale

import scala.collection._
import Util._

trait Label0 extends Function0[Term] with NodeLabel {
  val arity = 0

  def apply(): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply()
}

trait FreeLabel

case class FreeLabel0(name: String)(implicit val env: Environment) extends Label0 with FreeLabel {
  def apply(): Term = FreeNode0(this)
}

trait Label1 extends (Term => Term) with NodeLabel {
  val arity = 1

  def apply(_1: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head)
}

case class FreeLabel1(name: String)(implicit val env: Environment) extends Label1 with FreeLabel {
  def apply(_1: Term): Term = FreeNode1(this, _1)
}

trait Label2 extends ((Term, Term) => Term) with NodeLabel {
  val arity = 2

  def apply(_1: Term, _2: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head)

  def unapply(t: Term): Option[(Term, Term)] = t match {
    case n: Node2 if n.label == this => Some(n._1, n._2)
    case _ => None
  }
}

case class FreeLabel2(name: String)(implicit val env: Environment) extends Label2 with FreeLabel {
  def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
}

trait Label3 extends NodeLabel {
  val arity = 3

  def apply(_1: Term, _2: Term, _3: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head)
}

case class FreeLabel3(name: String)(implicit val env: Environment) extends Label3 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term = FreeNode3(this, _1, _2, _3)
}

trait Label4 extends NodeLabel {
  val arity = 4

  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head)
}

case class FreeLabel4(name: String)(implicit val env: Environment) extends Label4 with FreeLabel {
  def apply(_1: Term, _2: Term, _3: Term, _4: Term): Term = FreeNode4(this, _1, _2, _3, _4)
}

trait Label5 extends NodeLabel {
  val arity = 5

  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head, l.tail.tail.tail.tail.head)
}

trait Label6 extends NodeLabel {
  val arity = 6

  def apply(_1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term): Term

  protected def constructFromChildren(l: Iterable[Term]): Term = apply(l.head, l.tail.head, l.tail.tail.head, l.tail.tail.tail.head, l.tail.tail.tail.tail.head, l.tail.tail.tail.tail.tail.head)
}

trait Node0 extends Node {
  val label: Label0

  val isGround = true

  def innerUpdateAt(i: Int, t: Term): Term = throw new AssertionError("unreachable code")

  def iterator = Iterator.empty
}

case class FreeNode0(label: Label0) extends Node0

trait Node1 extends Node with Product1[Term] {
  val label: Label1

  lazy val isGround = _1.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t)
  }

  def iterator = Iterator(_1)
}

case class FreeNode1(label: Label1, _1: Term) extends Node1

trait Node2 extends Node with Product2[Term, Term] {
  val label: Label2

  lazy val isGround = _1.isGround && _2.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2)
    case 2 => label(_1, t)
  }

  def iterator = Iterator(_1, _2)
}

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2

trait Node3 extends Node with Product3[Term, Term, Term] {
  val label: Label3

  lazy val isGround = _1.isGround && _2.isGround && _3.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3)
    case 2 => label(_1, t, _3)
    case 3 => label(_1, _2, t)
  }

  def iterator = Iterator(_1, _2, _3)
}

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3

trait Node4 extends Node with Product4[Term, Term, Term, Term] {
  val label: Label4

  lazy val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3, _4)
    case 2 => label(_1, t, _3, _4)
    case 3 => label(_1, _2, t, _4)
    case 4 => label(_1, _2, _3, t)
  }

  def iterator = Iterator(_1, _2, _3, _4)
}

case class FreeNode4(label: Label4, _1: Term, _2: Term, _3: Term, _4: Term) extends Node4

trait Node5 extends Node with Product5[Term, Term, Term, Term, Term] {
  val label: Label5

  lazy val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3, _4, _5)
    case 2 => label(_1, t, _3, _4, _5)
    case 3 => label(_1, _2, t, _4, _5)
    case 4 => label(_1, _2, _3, t, _5)
    case 5 => label(_1, _2, _3, _4, t)
  }

  def iterator = Iterator(_1, _2, _3, _4, _5)
}

case class FreeNode5(label: Label5, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term) extends Node5

trait Node6 extends Node with Product6[Term, Term, Term, Term, Term, Term] {
  val label: Label6

  lazy val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround && _6.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2, _3, _4, _5, _6)
    case 2 => label(_1, t, _3, _4, _5, _6)
    case 3 => label(_1, _2, t, _4, _5, _6)
    case 4 => label(_1, _2, _3, t, _5, _6)
    case 5 => label(_1, _2, _3, _4, t, _6)
    case 6 => label(_1, _2, _3, _4, _5, t)
  }

  def iterator = Iterator(_1, _2, _3, _4, _5, _6)
}

case class FreeNode6(label: Label6, _1: Term, _2: Term, _3: Term, _4: Term, _5: Term, _6: Term) extends Node6

case class EqualityLabel(implicit val env: Environment) extends NameFromObject with Label2 {
  override def apply(_1: Term, _2: Term): Term = env.bottomize(_1, _2) {
    val Variable = env.Variable
    _1.label match {
      case `Variable` => new Binding(_1.asInstanceOf[Variable], _2)
      case _ => new Equality(_1, _2)
    }
  }
}

trait Substitution extends Term {
  def get(v: Variable): Option[Term]
}

private[kale] class Equality(val _1: Term, val _2: Term)(implicit env: Environment) extends Node2 {
  val label = env.Equality

  override def equals(other: Any) = other match {
    case that: Equality => this._1 == that._1 && this._2 == that._2
    case _ => false
  }
}

private[kale] class Binding(val variable: Variable, val term: Term)(implicit env: Environment) extends Equality(variable, term) with Substitution {
  assert(_1.isInstanceOf[Variable])

  def get(v: Variable) = if (_1 == v) Some(_2) else None
}

trait And extends Assoc

case class AndLabel(implicit val env: Environment) extends NameFromObject with AssocLabel {

  import env._

  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val l1: (Substitution, Iterable[Term]) = unwrap(_1)
      val l2: (Substitution, Iterable[Term]) = unwrap(_2)
      val allElements: Set[Term] = (l1._2.toSet ++ l2._2 + l1._1 + l2._1)
      Or(allElements
        .collect({ case Or.set(elements) => elements })
        .reduce(cartezianProduct))
    }
  }

  private def applyOnNonAnds(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val l1: (Substitution, Iterable[Term]) = unwrap(_1)
      val l2: (Substitution, Iterable[Term]) = unwrap(_2)
      Substitution(l1._1, l2._1) match {
        case `Bottom` => Bottom
        case s: Substitution => apply(s, l1._2 ++ l2._2)
        case _ => unreachable()
      }
    }
  }

  private def unwrap(t: Term): (Substitution, Iterable[Term]) = t match {
    case s: Substitution => (s, Iterable.empty)
    case and: AndOfSubstitutionAndTerms => (and.s, and.terms)
    case o => (Top, Iterable(o))
  }

  private def cartezianProduct(t1: Iterable[Term], t2: Iterable[Term]): Seq[Term] = {
    for (e1 <- t1.toSeq; e2 <- t2.toSeq) yield {
      applyOnNonAnds(e1, e2)
    }
  }

  override def apply(terms: Iterable[Term]): Term = {
    val disjunction = terms.collect({ case Or.set(elements) => elements }).reduce(cartezianProduct)
    Or(disjunction)

    //    val bindings: Map[Variable, Term] = terms.collect({ case Equality(v: Variable, t) => v -> t }).toMap
    //    val pureSubstitution = Substitution(bindings)
    //    val others: Iterable[Term] = terms.filter({ case Equality(v: Variable, t) => false; case _ => true })
    //    apply(pureSubstitution, others)
  }

  def apply(pureSubstitution: Substitution, others: Iterable[Term]): Term = {
    if (others.isEmpty) {
      pureSubstitution
    } else if (pureSubstitution.isEmpty && others.size == 1) {
      others.head
    } else {
      new AndOfSubstitutionAndTerms(pureSubstitution, others.toSet)
    }
  }
}

private[kale] final class AndOfSubstitutionAndTerms(val s: Substitution, val terms: Set[Term])(implicit env: Environment) extends Assoc {

  import env._

  assert(!terms.contains(Bottom))

  val label = And

  lazy val _1: Term = terms.head
  lazy val _2: Term = And(s, terms.tail)
  override val assocIterable: Iterable[Term] = Substitution.asList(s) ++ terms
}

case class SubstitutionLabel(implicit val env: Environment) extends NameFromObject with AssocLabel {

  import env._

  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val m1 = unwrap(_1)
      val m2 = unwrap(_2)
      if ((m1.keys.toSet & m2.keys.toSet).forall(v => m1(v) == m2(v))) {
        apply(m1 ++ m2)
      } else {
        Bottom
      }
    }
  }

  private def unwrap(t: Term) = t match {
    case Top => Map[Variable, Term]()
    case Equality(_1: Variable, _2) => Map[Variable, Term](_1.asInstanceOf[Variable] -> _2)
    case Substitution.map(m) => m
  }

  override def apply(l: Iterable[Term]) = l.foldLeft(Top: Term)(apply)

  def apply(m: Map[Variable, Term]): Substitution = m.size match {
    case 0 => Top
    case 1 => new Binding(m.head._1, m.head._2)
    case _ => new SubstitutionWithMultipleBindings(m)
  }

  object map {
    def unapply(t: Substitution): Option[Map[Variable, Term]] = t match {
      case `Top` => Some(Map[Variable, Term]())
      case b: Binding => Some(Map(b.variable -> b.term))
      case s: SubstitutionWithMultipleBindings => Some(s.m)
    }
  }

}

final class SubstitutionWithMultipleBindings(val m: Map[Variable, Term])(implicit env: Environment) extends And with Substitution {
  assert(m.size >= 2)

  import env._

  val label = Substitution
  lazy val _1 = Equality(m.head._1, m.head._2)
  lazy val _2 = Substitution(m.tail)

  override def equals(other: Any): Boolean = other match {
    case that: SubstitutionWithMultipleBindings => m == that.m
    case _ => false
  }

  override val hashCode: Int = label.hashCode

  def get(v: Variable) = m.get(v)

  override val assocIterable: Iterable[Term] = Substitution.asList(this)
}

case class OrLabel(implicit val env: Environment) extends NameFromObject with AssocLabel {

  import env._

  def apply(_1: Term, _2: Term): Term =
    unwrap(_1) | unwrap(_2) match {
      case s if s.isEmpty => Bottom
      case s if s.size == 1 => s.head
      case s => new OrWithAtLeastTwoElements(s)
    }

  def unwrap(t: Term): Set[Term] = t match {
    case o: OrWithAtLeastTwoElements => o.terms
    case `Bottom` => Set()
    case o => Set(o)
  }

  override def apply(l: Iterable[Term]): Term = l.foldLeft(Bottom: Term)(apply)

  object set {
    def unapply(t: Term): Some[Set[Term]] = Some(unwrap(t))
  }

}

private[this] class OrWithAtLeastTwoElements(val terms: Set[Term])(implicit env: Environment) extends Assoc {
  assert(terms.size > 1)

  import env._

  val label = Or

  lazy val _1 = terms.head
  lazy val _2 = Or(terms.tail.toSeq)
  override val assocIterable: Iterable[Term] = terms

  override def equals(other: Any): Boolean = other match {
    case that: OrWithAtLeastTwoElements => this.terms == that.terms
    case _ => false
  }
}

trait AssocLabel extends Label2 {
  def apply(l: Iterable[Term]): Term

  def apply(terms: Term*): Term = apply(terms)

  val thisthis = this

  def asList(t: Term) = t.label match {
    case `thisthis` => t.asInstanceOf[Assoc].assocIterable
    case _ => List(t)
  }

  val listUnapplier = new {
    def unapplySeq(t: Term): Seq[Term] = asList(t).toList
  }
}

trait HasId {
  val identity: Term
}

trait AssocWithIdLabel extends AssocLabel with HasId {
  def apply(_1: Term, _2: Term) = {
    val l1 = unwrap(_1)
    val l2 = unwrap(_2)
    apply(l1 ++ l2)
  }

  def unwrap(t: Term) = t match {
    case `identity` => List[Term]()
    case x if x.label == this => x.asInstanceOf[Assoc].assocIterable
    case y => List(y)
  }

  override def apply(list: Iterable[Term]): Term = list match {
    case l if l.isEmpty => identity
    case l if l.size == 1 => l.head
    case l => construct(l)
  }

  def construct(l: Iterable[Term]): Term
}

trait AssocWithoutIdLabel extends AssocLabel {
  // todo
}

trait Assoc extends Node2 {
  override val label: AssocLabel
  val assocIterable: Iterable[Term]
}

class AssocWithIdListLabel(val name: String, val identity: Term)(implicit val env: Environment) extends AssocWithIdLabel {
  override def construct(l: Iterable[Term]): Term = new AssocWithIdList(this, l)
}

case class AssocWithIdList(label: AssocLabel, assocIterable: Iterable[Term]) extends Assoc {
  override def _1: Term = assocIterable.head

  override def _2: Term = label(assocIterable.tail)
}

case class RewriteLabel(implicit val env: Environment) extends NameFromObject with Label2 {
  def apply(_1: Term, _2: Term) = new Rewrite(_1, _2)
}

case class Rewrite(_1: Term, _2: Term)(implicit env: Environment) extends Node2 {
  override val label = env.Rewrite
}
