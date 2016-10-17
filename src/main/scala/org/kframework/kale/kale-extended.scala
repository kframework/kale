package org.kframework.kale

import scala.collection._
import scala.Iterable

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

  val isGround = _1.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t)
  }

  def iterator = Iterator(_1)
}

case class FreeNode1(label: Label1, _1: Term) extends Node1

trait Node2 extends Node with Product2[Term, Term] {
  val label: Label2

  val isGround = _1.isGround && _2.isGround

  def innerUpdateAt(i: Int, t: Term): Term = i match {
    case 1 => label(t, _2)
    case 2 => label(_1, t)
  }

  def iterator = Iterator(_1, _2)
}

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2

trait Node3 extends Node with Product3[Term, Term, Term] {
  val label: Label3

  val isGround = _1.isGround && _2.isGround && _3.isGround

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

  val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround

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

  val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround

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

  val isGround = _1.isGround && _2.isGround && _3.isGround && _4.isGround && _5.isGround && _6.isGround

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

case class EqualityLabel(implicit val env: Environment) extends {
  val name = "="
} with Label2 with FormulaLabel {
  override def apply(_1: Term, _2: Term): Term = env.bottomize(_1, _2) {
    if (_1 == _2)
      env.Top
    else if (_1.isGround && _2.isGround) {
      env.Bottom
    } else {
      import StaticImplicits._
      val Variable = env.Variable
      _1.label match {
        case `Variable` if !_2.contains(_1) => createBinding(_1.asInstanceOf[Variable], _2)
        case _ => new Equality(_1, _2)
      }
    }
  }

  def createBinding(_1: Variable, _2: Term) = {
    import StaticImplicits._
    assert(!_2.contains(_1))
    new Binding(_1.asInstanceOf[Variable], _2)
  }
}

trait Substitution extends Term with (Term => Term) {
  def get(v: Variable): Option[Term]
  def apply(t: Term): Term
}

private[kale] class Equality(val _1: Term, val _2: Term)(implicit env: Environment) extends Node2 with BinaryInfix {
  val label = env.Equality

  override def equals(other: Any) = other match {
    case that: Equality => this._1 == that._1 && this._2 == that._2
    case _ => false
  }
}

private[kale] class Binding(val variable: Variable, val term: Term)(implicit env: Environment) extends Equality(variable, term) with Substitution with BinaryInfix {
  assert(_1.isInstanceOf[Variable])

  def get(v: Variable) = if (_1 == v) Some(_2) else None

  /**
    * Inefficient -- replace with some default version of ApplySubstitution
    */
  def apply(t: Term): Term = t match {
    case `variable` => term
    case Node(l, cs) => l((cs map apply).toIterable)
    case _ => t
  }

  override def toString: String = super[BinaryInfix].toString
}

trait And extends Assoc

case class AndLabel(implicit val env: Environment) extends {
  val name = "∧"
} with AssocWithIdLabel with FormulaLabel {

  import env._

  /**
    * normalizing
    */
  override def apply(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val substitutionAndTerms(sub1, terms1) = _1
      val substitutionAndTerms(sub2, terms2) = _2
      val allElements: Set[Term] = terms1.toSet ++ terms2 + sub1 + sub2
      Or(allElements map Or.asSet reduce cartezianProduct)
    }
  }

  /**
    * normalizing
    */
  override def apply(terms: Iterable[Term]): Term = {
    val disjunction = terms map Or.asSet reduce cartezianProduct
    Or(disjunction)

    //    val bindings: Map[Variable, Term] = terms.collect({ case Equality(v: Variable, t) => v -> t }).toMap
    //    val pureSubstitution = Substitution(bindings)
    //    val others: Iterable[Term] = terms.filter({ case Equality(v: Variable, t) => false; case _ => true })
    //    apply(pureSubstitution, others)
  }

  /**
    * normalizing
    */
  private def applyOnNonOrs(_1: Term, _2: Term): Term = {
    if (_1 == Bottom || _2 == Bottom)
      Bottom
    else {
      val substitutionAndTerms(sub1, terms1) = _1
      val substitutionAndTerms(sub2, terms2) = _2
      apply(sub1, sub2) match {
        case `Bottom` => Bottom
        case substitutionAndTerms(sub, terms) =>
          apply(sub, (terms1 ++ terms2 map sub) ++ terms)
        case _ => unreachable()
      }
    }
  }

  /**
    * not-normalizing
    */
  def apply(m: Map[Variable, Term]) = substitution(m)

  /**
    * normalizing
    */
  def apply(_1: Substitution, _2: Substitution): Term = substitution(_1, _2)

  /**
    * not-normalizing
    */
  def apply(pureSubstitution: Substitution, others: Iterable[Term]): Term = substitutionAndTerms(pureSubstitution, others)

  def asMap(t: Substitution): Map[Variable, Term] = t match {
    case `Top` => Map[Variable, Term]()
    case b: Binding => Map(b.variable -> b.term)
    case s: SubstitutionWithMultipleBindings => s.m
  }

  object substitution {

    /**
      * normalizing
      */
    def apply(_1: Substitution, _2: Substitution): Term = {
      // TODO: optimize to use the larger substitution as the first one
      val substitutionAndTerms(newSubs2, termsOutOfSubs2: Iterable[Term]) = _1(_2)

      val applyingTheSubsOutOf2To1 = newSubs2(_1).asInstanceOf[Substitution]

      val m1 = asMap(applyingTheSubsOutOf2To1)
      val m2 = asMap(newSubs2)

      val newSub: Substitution = substitution(m1 ++ m2)

      AndLabel.this.apply(newSub, termsOutOfSubs2)
    }

    /**
      * not-normalizing
      */
    def apply(m: Map[Variable, Term]): Substitution = m.size match {
      case 0 => Top
      case 1 => new Binding(m.head._1, m.head._2)
      case _ => new SubstitutionWithMultipleBindings(m)
    }

    def unapply(t: Term): Option[Map[Variable, Term]] = t match {
      case t: Substitution => Some(asMap(t))
      case _ => None
    }
  }

  def asSubstitutionAndTerms(t: Term): (Substitution, Iterable[Term]) = t match {
    case s: Substitution => (s, Iterable.empty)
    case and: AndOfSubstitutionAndTerms => (and.s, and.terms)
    case and: AndOfTerms => (Top, and.terms)
    case o => (Top, Iterable(o))
  }

  /**
    * Unwraps into a substitution and non-substitution terms
    */
  object substitutionAndTerms {
    /**
      * not normalizing
      */
    def apply(pureSubstitution: Substitution, otherTerms: Iterable[Term]): Term = {
      val others = otherTerms filterNot (_ == env.Top)

      assert(others forall { t => t == pureSubstitution(t) })

      if (others.isEmpty) {
        pureSubstitution
      } else if (pureSubstitution == Top && others.size == 1) {
        others.head
      } else {
        val terms = if (others.size > 1) new AndOfTerms(others.toSet) else others.head
        bottomize(others.toSeq: _*) {
          if (pureSubstitution == Top) {
            terms
          } else {
            new AndOfSubstitutionAndTerms(pureSubstitution, terms)
          }
        }
      }
    }

    def unapply(t: Term): Option[(Substitution, Iterable[Term])] = Some(asSubstitutionAndTerms(t))
  }

  private def cartezianProduct(t1: Iterable[Term], t2: Iterable[Term]): Seq[Term] = {
    for (e1 <- t1.toSeq; e2 <- t2.toSeq) yield {
      applyOnNonOrs(e1, e2)
    }
  }

  override def construct(l: Iterable[Term]): Term = ???

  override val identity: Term = Top
}

private[kale] final class AndOfTerms(val terms: Set[Term])(implicit env: Environment) extends And with Assoc {

  import env._

  assert(terms.size > 1, terms.toString())
  assert(!terms.contains(Bottom))
  assert(!terms.contains(Top))

  override val label = And
  override val assocIterable: Iterable[Term] = terms

  override def _1: Term = terms.head

  override def _2: Term = if (terms.size == 2) terms.tail.head else new AndOfTerms(terms.tail)

  override def equals(other: Any) = other match {
    case that: AndOfTerms => this.terms == that.terms
    case _ => false
  }
}

private[kale] final class AndOfSubstitutionAndTerms(val s: Substitution, val terms: Term)(implicit env: Environment) extends And with Assoc {

  import env._

  val label = And

  lazy val _1: Term = s
  lazy val _2: Term = terms
  override lazy val assocIterable: Iterable[Term] = And.asList(s) ++ And.asList(terms)
}

abstract class Named(val name: String)

final class SubstitutionWithMultipleBindings(val m: Map[Variable, Term])(implicit env: Environment) extends And with Substitution with BinaryInfix {
  assert(m.size >= 2)

  import env._

  val label = And
  lazy val _1 = Equality(m.head._1, m.head._2)
  lazy val _2 = And.substitution(m.tail)

  override def equals(other: Any): Boolean = other match {
    case that: SubstitutionWithMultipleBindings => m == that.m
    case _ => false
  }

  override val hashCode: Int = label.hashCode

  def get(v: Variable) = m.get(v)

  override val assocIterable: Iterable[Term] = And.asList(this)

  /**
    * Inefficient -- replace with some default version of ApplySubstitution
    */
  def apply(t: Term): Term = t match {
    case v: Variable => m.getOrElse(v, v)
    case Node(l, cs) => l((cs map apply).toIterable)
    case _ => t
  }

  override def toString: String = super[BinaryInfix].toString
}

case class OrLabel(implicit val env: Environment) extends {
  val name = "∨"
} with AssocLabel with FormulaLabel {

  import env._

  def apply(_1: Term, _2: Term): Term =
    asSet(_1) | asSet(_2) match {
      case s if s.isEmpty => Bottom
      case s if s.size == 1 => s.head
      case s => new OrWithAtLeastTwoElements(s)
    }

  def asSet(t: Term): Set[Term] = t match {
    case o: OrWithAtLeastTwoElements => o.terms
    case `Bottom` => Set()
    case o => Set(o)
  }

  object set {
    def unapply(t: Term): Option[Set[Term]] = Some(asSet(t))
  }

  override def apply(l: Iterable[Term]): Term = l.foldLeft(Bottom: Term)(apply)
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

  def asList(t: Term): Iterable[Term] = t.label match {
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

  // normalizing
  def apply(_1: Term, _2: Term) = {
    val l1 = asIterable(_1)
    val l2 = asIterable(_2)
    construct(l1 ++ l2)
  }

  def asIterable(t: Term): Iterable[Term] = t match {
    case `identity` => List[Term]()
    case x if x.label == this => x.asInstanceOf[Assoc].assocIterable
    case y => List(y)
  }

  // normalizing
  override def apply(list: Iterable[Term]): Term = list filterNot (_ == identity) match {
    case l if l.isEmpty => identity
    case l if l.size == 1 => l.head
    case l => (l fold identity) ((a, b) => apply(a, b))
  }

  def construct(l: Iterable[Term]): Term
}

trait AssocWithoutIdLabel extends AssocLabel {
  // todo
}

trait Assoc extends Node2 with BinaryInfix {
  override val label: AssocLabel
  val assocIterable: Iterable[Term]
}

class AssocWithIdListLabel(val name: String, val identity: Term)(implicit val env: Environment) extends AssocWithIdLabel {
  override def construct(l: Iterable[Term]): Term = new AssocWithIdList(this, l)
}

case class AssocWithIdList(label: AssocWithIdLabel, assocIterable: Iterable[Term]) extends Assoc {
  assert(assocIterable.forall(_ != label.identity))
  assert(assocIterable.forall(_.label != label))

  override def _1: Term = assocIterable.head

  override def _2: Term = label(assocIterable.tail)
}

case class RewriteLabel(implicit val env: Environment) extends {
  val name = "=>"
} with Label2 {
  def apply(_1: Term, _2: Term) = new Rewrite(_1, _2)
}

trait BinaryInfix {
  self: Node2 =>
  override def toString = _1 + " " + label.name + " " + _2
}

case class Rewrite(_1: Term, _2: Term)(implicit env: Environment) extends Node2 with BinaryInfix {
  override val label = env.Rewrite
}

class InvokeLabel(implicit val env: Environment) extends NameFromObject with Label1 {
  // the rewriter is initialized after the creation of the label to break the cycle when creating the rewriter for applying functions
  var rewriter: Rewriter = null
  override def apply(obj: Term): Term = Invoke(this, obj)
}

case class Invoke(label: InvokeLabel, _1: Term) extends Node1
