package kale

import scala.collection.{IterableLike, mutable}
import scala.language.implicitConversions

import LOGIC._

trait Label extends MemoizedHashCode {
  val module: Module
  val name: String

  override def equals(other: Any) = other match {
    case that: Label => this.module == that.module && this.name == that.name
    case _ => false
  }
  override def computeHashCode = module.hashCode * 7 + name.hashCode

  override def toString = name + "@" + module

  def unify(term: Term, that: Term) = this.module.unify(term, that)
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

sealed trait Term {
  val label: Label
  def iterator(): Iterator[Term]
  def unify(that: Term): Term = label.unify(this, that)
  // basic implementation -- override for performance
  def map(f: (Term) => Term): Term
}
trait Node extends Term {
  val label: NodeLabel
  def iterator: Iterator[Term]


  // = label(iterator.map(f).toSeq)
  override def toString = label + "(" + iterator.mkString(", ") + ")"
}

trait Leaf[T] extends Term {
  def iterator = Iterator.empty
  val label: LeafLabel[T]
  val value: T
  def map(f: (Term) => Term): Term = this
  override def toString = label + "(" + value + ")"
}

trait Module extends NameFromObject {
  def unify(a: Term, b: Term): Term

  trait InModule {
    val module: Module = Module.this
  }
  override def toString() = name
}

trait NameFromObject {
  val name = this.getClass.getName.drop(5).dropRight(1)
}

case class TokenLabel[T](module: Module, name: String) extends LeafLabel[T] {
  def apply(v: T) = Token(this, v)
}
case class Token[T](label: TokenLabel[T], value: T) extends Leaf[T]

trait Node0Label extends (=> Term) with NodeLabel {
  def apply(): Term = Node0(this)
}
trait Node1Label extends (Term => Term) with NodeLabel {
  def apply(_1: Term): Term = Node1(this, _1)
}
trait Node2Label extends ((Term, Term) => Term) with NodeLabel {
  def apply(_1: Term, _2: Term): Term
}
trait SimpleNode2Label {
  self: Node2Label =>
  def apply(_1: Term, _2: Term): Term = Node2(this, _1, _2)
}

trait Node3Label extends NodeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term = Node3(this, _1, _2, _3)
}

trait Node0 extends Node {
  val label: Node0Label
  def iterator = Iterator.empty
  override def map(f: (Term) => Term): Term = label()
}
object Node0 {
  def apply(label: Node0Label) = Node0Implementation(label)
}

case class Node0Implementation(label: Node0Label) extends Node0

case class Node1(label: Node1Label, _1: Term) extends Node with Product1[Term] {
  def iterator = Iterator(_1)
  override def map(f: (Term) => Term): Term = label(f(_1))
}
trait Node2 extends Node with Product2[Term, Term] {
  val label: Node2Label
  def iterator = Iterator(_1, _2)
  override def map(f: (Term) => Term): Term = label(f(_1), f(_2))
}
object Node2 {
  def apply(label: Node2Label, _1: Term, _2: Term) = Node2Implementation(label, _1, _2)
}
case class Node2Implementation(label: Node2Label, _1: Term, _2: Term) extends Node2
case class Node3(label: Node3Label, _1: Term, _2: Term, _3: Term) extends Node {
  def iterator = Iterator(_1, _2, _3)
  override def map(f: (Term) => Term): Term = label(f(_1), f(_2), f(_3))
}


trait CollectionNode[C <: IterableLike[Term, C]] extends Node {
  val collection: C
}

object Node {
  def apply(label: Node0Label) = Node0(label)
  def apply(label: Node1Label, _1: Term) = Node1(label, _1)
  def apply(label: Node2Label, _1: Term, _2: Term) = Node2(label, _1, _2)
  def apply(label: Node3Label, _1: Term, _2: Term, _3: Term) = Node3(label, _1, _2, _3)
}

case class Operator0Label[T](module: Module, name: String, elm: TokenLabel[T], f: () => T) extends Node0Label {
  override def apply(): Term = elm(f())
}
trait Function1Label[A, R] extends Node1Label {
  val aLabel: TokenLabel[A]
  val rLabel: TokenLabel[R]
  val f: A => R
  override def apply(_1: Term): Term = _1 match {
    case aLabel(a) => rLabel(f(a))
    case _ => Node(this, _1)
  }
}

case class Operator1Label[T](module: Module, name: String, elmLabel: TokenLabel[T], f: T => T)
  extends Function1Label[T, T] {
  val aLabel = elmLabel
  val rLabel = elmLabel
}

trait Function2Label[A, B, R] extends Node2Label {
  val aLabel: TokenLabel[A]
  val bLabel: TokenLabel[B]
  val rLabel: TokenLabel[R]
  val f: (A, B) => R
  override def apply(_1: Term, _2: Term): Term = (_1, _2) match {
    case (aLabel(a), bLabel(b)) => rLabel(f(a, b))
    case _ => Node(this, _1, _2)
  }
}

abstract class Operator2Label[T](val name: String, val f: (T, T) => T) extends Function2Label[T, T, T] {
  val elmLabel: TokenLabel[T]
  lazy val aLabel = elmLabel
  lazy val rLabel = elmLabel
  lazy val bLabel = elmLabel
}

object LOGIC extends Module {
  object Variable extends LeafLabel[String] with InModule with NameFromObject
  case class Variable(value: String) extends Leaf[String] {
    override val label: LeafLabel[String] = Variable
  }

  def Truth(b: Boolean) = if (b) True else False

  object And extends Node2Label with InModule {
    override val name = "And"
    override def apply(_1: Term, _2: Term): Term = LOGIC.this.unify(_1, _2)
  }

  object OrModule extends ASSOC_LIST("op", False) {
    override val name = "Or"
    override def theOp(_1: Term, _2: Term): Either[Term, (Term, Term)] = (_1, _2) match {
      case (a, True) => Left(True)
      case (True, a) => Left(True)
      case (False, a) => Left(a)
      case (a, False) => Left(a)
      case _ => Right(_1, _2)
    }
  }

  val Or = OrModule.op

  object TrueLabel extends Node0Label with InModule {
    override val name: String = "True"
  }
  val True = Node0(TrueLabel)
  object FalseLabel extends Node0Label with InModule {
    override val name: String = "False"
  }
  val False = Node0(FalseLabel)

  object Rewrite extends Node2Label with InModule {
    override val name: String = "=>"
    override def apply(_1: Term, _2: Term): Term = ???
  }
  object Substitutions extends MAP(",", "|->", new Node0Label with InModule {
    override val name: String = ".Substitutions"
  }.apply())

  def unify(a: Term, b: Term): Term = {
    (a, b) match {
      case (a, True) => a
      case (True, a) => a
      case (False, _) => False
      case (_, False) => False
      case (v: Variable, b) => Substitutions.Pair(v, b)
      case (a, b) => a.unify(b)
    }
  }
}

//object size extends Node1Label  {
//  val name = "size"
//  override def apply(_1: Term): Term = _1 match {
//    case Term(l, _) if ... => l.asCollection(l).size
//    case e => 1
//    case unit => 0
//  }
//}

trait AssocModule
  extends Module {
  type C <: IterableLike[Term, C]
  val opLabel: String
  val unit: Term
  def newBuilder(): mutable.Builder[Term, C]

  def theOp(a: Term, b: Term): Either[Term, (Term, Term)] = if (a == unit) Left(b) else Right((a, b))

  object op extends Node2Label with InModule {
    def apply(s: Iterable[Term]): Term = {
      var last = unit
      val iterator = s.iterator
      val builder = newBuilder()
      while (iterator.hasNext) {
        theOp(last, iterator.next()) match {
          case Right((a, b)) => builder += a; last = b
          case Left(a) => last = a
        }
      }
      if (last != unit) builder += last
      val res = builder.result()
      res.size match {
        case 0 => unit
        case 1 => res.head
        case _ => NeCollectionNode(res)
      }
    }

    def unapply(t: Term): Option[C] = Some(asCollection(t))

    override def apply(_1: Term, _2: Term): Term = apply(Iterable(_1, _2))

    implicit def asCollection(t: Term): C = t match {
      case `unit` => newBuilder().result()
      case t: NeCollectionNode if t.label == this => t.collection
      case _ => (newBuilder() += t).result()
    }
    val unit = AssocModule.this.unit
    val name: String = opLabel
  }

  case class NeCollectionNode(collection: C) extends CollectionNode[C] with Node2 {
    val label = op
    override lazy val _1: Term = collection.head
    override lazy val _2: Term = ??? // label(collection.tail)
    override def map(f: (Term) => Term): Term = label(collection.iterator.map(f).toIterable)
  }

  def matchContents(ksL: Seq[Term], ksR: Seq[Term]): Term = {
    val res = (ksL, ksR) match {
      case (Seq(), Seq()) => True
      case (headL +: tailL, headR +: tailR) if headL == tailL => matchContents(tailL, tailR)
      case (headL +: tailL, ksR) =>
        (0 to ksR.size)
          .map { index => (ksR.take(index), ksR.drop(index)) }
          .map {
            case (List(oneElement), suffix) =>
              And(headL.unify(oneElement), matchContents(tailL, suffix))
            case (prefix, suffix) =>
              And(headL.unify(op(prefix)), matchContents(tailL, suffix))
          }
          .fold(False)({ (a, b) => Or(a, b) })

      case other => False
    }
    res
  }
}

class SET(val opLabel: String, val unit: Term) extends AssocModule {
  type C = Set[Term]
  override def newBuilder = Set.newBuilder[Term]
  override def unify(a: Term, b: Term): Term = ???
}

class ASSOC_LIST(val opLabel: String, val unit: Term) extends AssocModule {
  def this(opLabel: String) = this(opLabel, new Node0Label with InModule {val name: String = "." + opLabel}.apply())

  type C = List[Term]
  override def newBuilder() = List.newBuilder[Term]

  def unify(a: Term, b: Term): Term = if (a.label == op) {
    matchContents(op.asCollection(a), op.asCollection(b))
  } else {
    False
  }
}

class MAP(val opLabel: String, val pairLabel: String, val unit: Term, falseOnDuplicate: Boolean = false) extends
AssocModule {
  type C = MapOfPairs
  object Pair extends Node2Label with InModule {
    override val name = pairLabel
    override def apply(_1: Term, _2: Term): Term = Node2(this, _1, _2)
  }
  case class MapOfPairs(m: Map[Term, Term]) extends IterableLike[Term, MapOfPairs] {
    override def iterator: Iterator[Term] = m.map({ case (a, b) => Pair(a, b) }).iterator
    override protected[this] def newBuilder = List.newBuilder[Term].mapResult(l =>
      MapOfPairs(l map { case Pair(a, b) => (a, b) } toMap))
    override def seq: TraversableOnce[Term] = m map { case (x, y) => Pair(x, y) }
  }

  override def newBuilder() = {
    val b = Map.newBuilder[Term, Term]
    new mutable.Builder[Term, MapOfPairs] {
      override def +=(elem: Term): this.type = elem match {
        case t: Node2 if t.label == Pair => b.+=((t._1, t._2)); this
        case _ => throw new Exception("Expected a Pair but got something else: " + elem)
      }
      override def clear(): Unit = b.clear()
      override def result(): MapOfPairs = MapOfPairs(b.result())
    }
  }

  override def unify(a: Term, b: Term): Term = {
    val aC: MapOfPairs = op.asCollection(a)
    val bC: MapOfPairs = op.asCollection(b)
    if (!falseOnDuplicate || (aC.m.keySet.intersect(bC.m.keySet).isEmpty)) {
      op((aC.m ++ bC.m) map { case (a, b) => Pair(a, b) })
    }
    else {
      False
    }
  }
}

object StructuralMatch {
  def apply(a: Term, b: Term) = (a, b) match {
    case (Token(aL, aV), Token(bL, bV)) => Truth(aL == bL && aV == bV)
    case _ if a.label == b.label => a.iterator().zip(b.iterator()).map(p => p._1.unify(p._2)).reduce(And)
    case _ => False
  }
}

object INT extends Module {
  val Int = TokenLabel[scala.Int](this, "Int")
  protected trait IntOp {
    val elmLabel: TokenLabel[Int] = Int
  }
  object - extends Operator2Label[scala.Int]("-", _ - _) with IntOp with InModule
  object + extends Operator2Label[scala.Int]("+", _ + _) with IntOp with InModule
  override def unify(a: Term, b: Term): Term = StructuralMatch(a, b)
}

object KSEQ extends ASSOC_LIST("~>")

object ID extends Module {
  object Id extends LeafLabel[String] with InModule with NameFromObject
  case class Id(value: String) extends Leaf[String] {
    override val label: LeafLabel[String] = Id
  }
  override def unify(a: Term, b: Term): Term = ???
}
