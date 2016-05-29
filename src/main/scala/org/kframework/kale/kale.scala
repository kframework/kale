package org.kframework.kale

import scala.collection._
import scala.language.implicitConversions

object UniqueId {
  var nextId = 0

  def apply(): Int = {
    nextId += 1
    nextId - 1
  }
}

trait UniqueId {
  val id = UniqueId()
}

trait Label extends MemoizedHashCode {
  val name: String
  val id: Int

  override def equals(other: Any) = other match {
    case that: Label => this.name == that.name
    case _ => false
  }

  override def computeHashCode = name.hashCode

  override def toString = name
}

trait NodeLabel extends Label {
  def unapplySeq(t: Term): Option[Seq[Term]] = t match {
    case t: Node if t.label == this => Some(t.iterator.toSeq)
    case _ => None
  }
}

trait LeafLabel[T] extends Label {
  def apply(t: T): Term

  def unapply(t: Term): Option[T] = t match {
    case t: Leaf[T] if t.label == this => Some(t.value)
    case _ => None
  }
}

trait Term extends Iterable[Term] {
  def updateAt(i: Int)(t: Term): Term

  val label: Label

  private var att: Any = null

  def setHiddenAttDONOTUSE(att: Any) = this.att = att

  def getHiddenAttDONOTUSE = this.att

  def iterator(): Iterator[Term]

  override def hashCode = label.hashCode
}

trait Node extends Term with Product {
  val label: NodeLabel

  def updateAt(i: Int)(t: Term): Term = if (i <= 0 || i > productArity) {
    throw new IndexOutOfBoundsException(label + " has " + productArity + " children. Trying to update index _" + i)
  } else {
    innerUpdateAt(i, t)
  }

  protected def innerUpdateAt(i: Int, t: Term): Term

  def iterator: Iterator[Term]

  override def toString = label + "(" + iterator.mkString(", ") + ")"
}

trait Leaf[T] extends Term {
  def iterator = Iterator.empty

  def updateAt(i: Int)(t: Term): Term = throw new IndexOutOfBoundsException("Leaves have no children. Trying to update index _" + i)

  val label: LeafLabel[T]
  val value: T

  override def toString = label + "(" + value.toString + ")"
}

trait NameFromObject {
  val name = this.getClass.getName.drop(5).dropRight(1)
}

trait ConstantLabel[T] extends LeafLabel[T] with NameFromObject with UniqueId {
  def apply(v: T) = Constant(this, v)
}

case class Constant[T](label: ConstantLabel[T], value: T) extends Leaf[T] {
  override def toString = value.toString
}

object Variable extends LeafLabel[String] with NameFromObject with UniqueId {
  override def apply(name: String): Variable = SimpleVariable(name)
}

trait Variable extends Leaf[String] {
  val label = Variable
  val name: String
  lazy val value = name
}

case class SimpleVariable(name: String) extends Variable

trait Hooked {
  def f(t: Term): Term
}

object Truth extends LeafLabel[Boolean] with NameFromObject with UniqueId {
  def apply(v: Boolean) = if (v) Top else Bottom
}

class Truth(val value: Boolean) extends Leaf[Boolean] {
  val label = Truth
}

object Top extends Truth(true) with Substitution {
  override def get(v: Variable): Option[Term] = None

  override def toString = "⊤"
}

object Bottom extends Truth(false) {
  override def toString = "⊥"
}

import transformer._

object SubstitutionApplication {
  def apply(pieces: Set[Unary.Piece[SubstitutionApplication]], maxId: Int)(s: Substitution) = new SubstitutionApplication(pieces, maxId)(s)
}

class SubstitutionApplication(pieces: Set[Unary.Piece[SubstitutionApplication]], maxId: Int)(s: Substitution) extends Unary.Application[SubstitutionApplication](pieces, maxId) {
  def get(v: Variable): Option[Term] = s.get(v)

  def apply(t: Term) = arr(t.label.id) match {
    case null => Bottom
    case f => f(t)
  }
}

object ApplySubstitution {

  def apply(labels: Set[Label]): Substitution => SubstitutionApplication = {
    val maxId = labels.map(_.id).max + 1
    val setOfUnaryPieces = labels.map({
      case `Variable` => Unary.Piece(Variable, Var)
      case l: Label0 => Unary.Piece(l, Node0)
      case l: Label1 => Unary.Piece(l, Node1)
      case l: Label2 => Unary.Piece(l, Node2)
      case l: Label3 => Unary.Piece(l, Node3)
      case l: Label4 => Unary.Piece(l, Node4)
      case l: ConstantLabel[_] => Unary.Piece(l, Constant)
    })

    SubstitutionApplication(setOfUnaryPieces, maxId)
  }

  object Node0 extends Unary.Function[Node0, Node0, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node0) = t
  }

  object Node1 extends Unary.Function[Node1, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node1) = t.label(solver(t._1))
  }

  object Node2 extends Unary.Function[Node2, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node2) = t.label(solver(t._1), solver(t._2))
  }

  object Node3 extends Unary.Function[Node3, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node3) = t.label(solver(t._1), solver(t._2), solver(t._3))
  }

  object Node4 extends Unary.Function[Node4, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(t: Node4) = t.label(solver(t._1), solver(t._2), solver(t._3), solver(t._4))
  }

  object Var extends Unary.Function[Variable, Term, SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(v: Variable) = solver.get(v).getOrElse(v)
  }

  object Constant extends Unary.Function[Constant[_], Constant[_], SubstitutionApplication] {
    def f(solver: SubstitutionApplication)(a: Constant[_]) = a
  }

}

object SimpleMatcher {

  def apply(labels: Set[Label]): Binary.Application = {
    val variableXlabel = labels.map(Binary.Piece(Variable, _, SimpleMatcher.VarLeft))
    val freeLikeLabelXfreeLikeLabel = labels.collect({
      case l: FreeLabel0 => Binary.Piece(l, l, SimpleMatcher.FreeNode0FreeNode0)
      case l: FreeLabel1 => Binary.Piece(l, l, SimpleMatcher.FreeNode1FreeNode1)
      case l: FreeLabel2 => Binary.Piece(l, l, SimpleMatcher.FreeNode2FreeNode2)
      case l: FreeLabel3 => Binary.Piece(l, l, SimpleMatcher.FreeNode3FreeNode3)
      case l: FreeLabel4 => Binary.Piece(l, l, SimpleMatcher.FreeNode4FreeNode4)
      case l: ConstantLabel[_] => Binary.Piece(l, l, SimpleMatcher.Constants)
    })

    val assoc = labels.flatMap({
      case l: AssocLabel =>
        labels.collect({ case ll if !ll.isInstanceOf[Variable] => Binary.Piece(l, ll, SimpleMatcher.AssocTerm) })
      case _ => Set[Binary.Piece]()
    }).toSet

    val anywhereContextMatchers = labels.map(Binary.Piece(AnywhereContext, _, AnywhereContextMatcher))

    new Binary.Application(variableXlabel | freeLikeLabelXfreeLikeLabel | assoc | anywhereContextMatchers, labels.map(_.id).max + 1)
  }

  object FreeNode0FreeNode0 extends Binary.Function[Node0, Node0, Top.type] {
    def f(solver: Binary.State)(a: Node0, b: Node0) = Top
  }

  object FreeNode1FreeNode1 extends Binary.Function[Node1, Node1, Term] {
    def f(solver: Binary.State)(a: Node1, b: Node1) = solver(a._1, b._1)
  }

  object FreeNode2FreeNode2 extends Binary.Function[Node2, Node2, Term] {
    def f(solver: Binary.State)(a: Node2, b: Node2) = And(solver(a._1, b._1), solver(a._2, b._2))
  }

  object FreeNode3FreeNode3 extends Binary.Function[Node3, Node3, Term] {
    def f(solver: Binary.State)(a: Node3, b: Node3) = And(List(solver(a._1, b._1), solver(a._2, b._2), solver(a._3, b._3)))
  }

  object FreeNode4FreeNode4 extends Binary.Function[Node4, Node4, Term] {
    def f(solver: Binary.State)(a: Node4, b: Node4) = And(List(solver(a._1, b._1), solver(a._2, b._2), solver(a._3, b._3), solver(a._4, b._4)))
  }

  def matchContents(l: AssocLabel, ksLeft: Iterable[Term], ksRight: Iterable[Term])(implicit solver: Binary.State): Term = {
    val res = (ksLeft.toSeq, ksRight.toSeq) match {
      case (Seq(), Seq()) => Top
      case ((v: Variable) +: tailL, ksR) =>
        (0 to ksR.size)
          .map { index => (ksR.take(index), ksR.drop(index)) }
          .map { case (prefix, suffix) => And(Equality(v, l(prefix)), matchContents(l, tailL, suffix)) }
          .fold(Bottom)({ (a, b) => Or(a, b) })
      case (left, right) if left.nonEmpty && right.nonEmpty => And(solver(left.head, right.head), matchContents(l, left.tail, right.tail): Term)
      case other => Bottom
    }
    res
  }


  object AssocTerm extends Binary.Function[Assoc, Term, Term] {
    def f(solver: Binary.State)(a: Assoc, b: Term) = {
      val asList = a.label.asList _
      val l1 = asList(a)
      val l2 = asList(b)
      matchContents(a.label, l1, l2)(solver)
    }
  }

  object TermAssoc extends Binary.Function[Term, Assoc, Term] {
    def f(solver: Binary.State)(a: Term, b: Assoc) = {
      val asList = b.label.asList _
      val l1 = asList(a)
      val l2 = asList(b)
      matchContents(b.label, l1, l2)(solver)
    }
  }

  object VarLeft extends Binary.Function[Variable, Term, Term] {
    def f(solver: Binary.State)(a: Variable, b: Term) = Equality(a.asInstanceOf[Variable], b)
  }

  object Constants extends Binary.Function[Constant[_], Constant[_], Term] {
    override def f(solver: Binary.State)(a: Constant[_], b: Constant[_]) =
      Truth(a.value == b.value)
  }

}

object Rewriter {
  def apply(substitutioner: Substitution => SubstitutionApplication, matcher: Binary.Application)(rules: Set[Rewrite]) =
    new Rewriter(substitutioner, matcher, rules)
}

class Rewriter(substitutioner: Substitution => SubstitutionApplication, matcher: Binary.Application, rules: Set[Rewrite]) {
  def executionStep(obj: Term): Term = {
    rules.toStream.map(r => (matcher(r._1, obj), r._2)).find(_._1 != Bottom) match {
      case Some((substitutions, rhs)) =>
        val oneSubstitutuion = Or.unwrap(substitutions).head.asInstanceOf[Substitution]
        substitutioner(oneSubstitutuion).apply(rhs)
      case None => Bottom
    }
  }

  def searchStep(obj: Term): Term = {
    Or(rules.map(r => (matcher(r._1, obj), r._2)).flatMap({
      case (Bottom, _) => Set[Term]()
      case (or, rhs) =>
        val substitutions: Set[Substitution] = Or.unwrap(or).asInstanceOf[Set[Substitution]]
        substitutions.map(substitutioner).map(_ (rhs))
    }))
  }
}
