package kale

import scala.collection.{IterableLike, mutable}
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

sealed trait Term extends Iterable[Term] {
  val label: Label

  def iterator(): Iterator[Term]
}

trait Node extends Term {
  val label: NodeLabel

  def iterator: Iterator[Term]

  override def toString = label + "(" + iterator.mkString(", ") + ")"
}

trait Leaf[T] extends Term {
  def iterator = Iterator.empty

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

trait Label0 extends Function0[Term] with NodeLabel {
  def apply(): Term
}


case class FreeLabel0(id: Int, name: String) extends Label0 {
  def apply(): Term = FreeNode0(this)
}

trait Label1 extends (Term => Term) with NodeLabel {
  def apply(_1: Term): Term
}

case class FreeLabel1(id: Int, name: String) extends Label1 {
  def apply(_1: Term): Term = FreeNode1(this, _1)
}

trait Label2 extends ((Term, Term) => Term) with NodeLabel {
  def apply(_1: Term, _2: Term): Term
}

case class FreeLabel2(id: Int, name: String) extends Label2 {
  def apply(_1: Term, _2: Term): Term = FreeNode2(this, _1, _2)
}

trait Label3 extends NodeLabel {
  def apply(_1: Term, _2: Term, _3: Term): Term
}

case class FreeLabel3(id: Int, name: String) extends Label3 {
  def apply(_1: Term, _2: Term, _3: Term): Term = FreeNode3(this, _1, _2, _3)
}

trait Node0 extends Node {
  val label: Label0

  def iterator = Iterator.empty
}

case class FreeNode0(label: Label0) extends Node0

trait Node1 extends Node with Product1[Term] {
  val label: Label1

  def iterator = Iterator(_1)
}

case class FreeNode1(label: Label1, _1: Term) extends Node1

trait Node2 extends Node with Product2[Term, Term] {
  val label: Label2

  def iterator = Iterator(_1, _2)
}

case class FreeNode2(label: Label2, _1: Term, _2: Term) extends Node2

trait Node3 extends Node with Product3[Term, Term, Term] {
  val label: Label3

  def iterator = Iterator(_1, _2, _3)
}

case class FreeNode3(label: Label3, _1: Term, _2: Term, _3: Term) extends Node3

object Variable extends LeafLabel[String] with NameFromObject with UniqueId

case class Variable(name: String) extends Leaf[String] {
  override val label = Variable
  val value = name
}

object Truth extends LeafLabel[Boolean] with NameFromObject with UniqueId

case class Truth(value: Boolean) extends Leaf[Boolean] with MatcherSolution {
  val label = Truth
}

object Top extends Truth(true)

object Bottom extends Truth(false)

object Binding extends Label2 with NameFromObject with UniqueId

trait MatcherSolution extends Term

case class Binding(_1: Term, _2: Term) extends Node2 with MatcherSolution {
  assert(_1.isInstanceOf[Variable])
  val label = Binding
}

object Substitution extends Label2 with NameFromObject with UniqueId {
  override def apply(_1: Term, _2: Term): Term = {
    val m1 = unwrap(_1)
    val m2 = unwrap(_2)
    if ((m1.keys.toSet & m2.keys.toSet).forall(v => m1(v) == m2(v))) {
      apply(m1 ++ m2)
    } else {
      Bottom
    }
  }

  def unwrap(t: Term) = t match {
    case Top => Map[Variable, Term]()
    case Binding(_1, _2) => Map[Variable, Term](_1.asInstanceOf[Variable] -> _2)
    case Substitution(m) => m
  }

  def apply(m: Map[Variable, Term]): MatcherSolution = m.size match {
    case 0 => Top
    case 1 => Binding(m.head._1, m.head._2)
    case _ => new Substitution(m)
  }

  def unapply(arg: Substitution): Option[Map[Variable, Term]] = Some(arg.m)
}

final class Substitution(val m: Map[Variable, Term]) extends Node2 with MatcherSolution {
  assert(m.size >= 2)
  val label = Substitution
  lazy val _1 = Binding(m.head._1, m.head._2)
  lazy val _2 = Substitution(m.tail)

  override def equals(other: Any): Boolean = other match {
    case that: Substitution => m == that.m
    case _ => false
  }

  override def hashCode(): Int = label.hashCode
}

trait Hook {
  val label: Label
  val f: Term => Term
}

trait UnifierFunction[Left <: Term, Right <: Term, Result <: Term] extends (DispatchState => ((Term, Term) => Term)) {
  def apply(solver: DispatchState) = { (a: Term, b: Term) => f(solver)(a.asInstanceOf[Left], b.asInstanceOf[Right]) }

  def foo(solver: DispatchState)(a: Term, b: Term): Term = f(solver)(a.asInstanceOf[Left], b.asInstanceOf[Right])

  def f(solver: DispatchState)(a: Left, b: Right): Result
}

case class UnifierPiece(leftLabel: Label, rightLabel: Label, f: DispatchState => (Term, Term) => Term)

trait DispatchState {
  def apply(left: Term, right: Term): Term
}

class Dispatch(pieces: Set[UnifierPiece], maxId: Int) extends DispatchState {
  val arr: Array[Array[(Term, Term) => (Term)]] =
    (0 until maxId).map({ i => new Array[(Term, Term) => (Term)](maxId) }).toArray

  for (p <- pieces) {
    arr(p.leftLabel.id)(p.rightLabel.id) = p.f(this)
  }

  def apply(left: Term, right: Term): Term = {
    val u = arr(left.label.id)(right.label.id)
    if(u != null)
      u(left, right)
    else
      Bottom
  }
}


object SimpleMatcher {

  object FreeNode0FreeNode0 extends UnifierFunction[Node0, Node0, Top.type] {
    def f(solver: DispatchState)(a: Node0, b: Node0) = Top
  }

  object FreeNode1FreeNode1 extends UnifierFunction[Node1, Node1, Term] {
    def f(solver: DispatchState)(a: Node1, b: Node1) = solver(a._1, b._1)
  }

  object FreeNode2FreeNode2 extends UnifierFunction[Node2, Node2, Term] {
    def f(solver: DispatchState)(a: Node2, b: Node2) = Substitution(solver(a._1, b._1), solver(a._2, b._2))
  }

  object VarLeft extends UnifierFunction[Variable, Term, MatcherSolution] {
    def f(solver: DispatchState)(a: Variable, b: Term) = Binding(a.asInstanceOf[Variable], b)
  }

  object Constants extends UnifierFunction[Constant[_], Constant[_], MatcherSolution] {
    override def f(solver: DispatchState)(a: Constant[_], b: Constant[_]) =
      Truth(a.value == b.value)
  }

}


//case class Operator0Label[T](name: String, elm: ConstantLabel[T], f: () => T) extends Node0Label {
//  override def apply(): Term = elm(f())
//}

//trait Function1Label[A, R] extends Node1Label {
//  val aLabel: TokenLabel[A]
//  val rLabel: TokenLabel[R]
//  val f: A => R
//
//  override def apply(_1: Term): Term = _1 match {
//    case aLabel(a) => rLabel(f(a))
//    case _ => Node(this, _1)
//  }
//}

//case class Operator1Label[T](module: Module, name: String, elmLabel: TokenLabel[T], f: T => T)
//  extends Function1Label[T, T] {
//  val aLabel = elmLabel
//  val rLabel = elmLabel
//}

//trait Function2Label[A, B, R] extends Node2Label {
//  val aLabel: ConstantLabel[A]
//  val bLabel: ConstantLabel[B]
//  val rLabel: ConstantLabel[R]
//  val f: (A, B) => R
//
//  override def apply(_1: Term, _2: Term): Term = (_1, _2) match {
//    case (aLabel(a), bLabel(b)) => f(a, b) match {
//      case Some(x) => rLabel(x)
//      case None => FreeNode2(this, _1, _2)
//    }
//    case _ => FreeNode2(this, _1, _2)
//  }
//}
//
//abstract class Operator2Label[T](val name: String, val f: (T, T) => Option[T]) extends Function2Label[T, T, T] {
//  val elmLabel: ConstantLabel[T]
//  lazy val aLabel = elmLabel
//  lazy val rLabel = elmLabel
//  lazy val bLabel = elmLabel
//}

//object LOGIC extends Module {
//
//  object Variable extends LeafLabel[String] with InModule with NameFromObject
//
//  case class Variable(value: String) extends Leaf[String] {
//    override val label: LeafLabel[String] = Variable
//  }
//
//  def Truth(b: Boolean) = if (b) True else False
//
//  object And extends Node2Label with InModule {
//    override val name = "And"
//
//    override def apply(_1: Term, _2: Term): Term = LOGIC.this.unify(_1, _2)
//  }
//
//  object OrModule extends ASSOC_LIST("op", False) {
//    override val name = "Or"
//
//    override def theOp(_1: Term, _2: Term): Either[Term, (Term, Term)] = (_1, _2) match {
//      case (a, True) => Left(True)
//      case (True, a) => Left(True)
//      case (False, a) => Left(a)
//      case (a, False) => Left(a)
//      case _ => Right(_1, _2)
//    }
//  }
//
//  val Or = OrModule.op
//
//  object TrueLabel extends Node0Label with InModule {
//    override val name: String = "True"
//  }
//
//  val True = Node0(TrueLabel)
//
//  object FalseLabel extends Node0Label with InModule {
//    override val name: String = "False"
//  }
//
//  val False = Node0(FalseLabel)
//
//  object Rewrite extends Node2Label with InModule {
//    override val name: String = "=>"
//
//    override def apply(_1: Term, _2: Term): Term = ???
//  }
//
//  object Substitutions extends MAP(",", "|->", new Node0Label with InModule {
//    override val name: String = ".Substitutions"
//  }.apply())
//
//  def unify(a: Term, b: Term): Term = {
//    (a, b) match {
//      case (a, True) => a
//      case (True, a) => a
//      case (False, _) => False
//      case (_, False) => False
//      case (v: Variable, b) => Substitutions.Pair(v, b)
//      case (a, b) => a.unify(b)
//    }
//  }
//}
//
////object size extends Node1Label  {
////  val name = "size"
////  override def apply(_1: Term): Term = _1 match {
////    case Term(l, _) if ... => l.asCollection(l).size
////    case e => 1
////    case unit => 0
////  }
////}
//
//trait AssocModule
//  extends Module {
//  type C <: IterableLike[Term, C]
//  val opLabel: String
//  val unit: Term
//
//  def newBuilder(): mutable.Builder[Term, C]
//
//  def theOp(a: Term, b: Term): Either[Term, (Term, Term)] = if (a == unit) Left(b) else Right((a, b))
//
//  object op extends Node2Label with InModule {
//    def apply(s: Iterable[Term]): Term = {
//      var last = unit
//      val iterator = s.iterator
//      val builder = newBuilder()
//      while (iterator.hasNext) {
//        theOp(last, iterator.next()) match {
//          case Right((a, b)) => builder += a; last = b
//          case Left(a) => last = a
//        }
//      }
//      if (last != unit) builder += last
//      val res = builder.result()
//      res.size match {
//        case 0 => unit
//        case 1 => res.head
//        case _ => NeCollectionNode(res)
//      }
//    }
//
//    def unapply(t: Term): Option[C] = Some(asCollection(t))
//
//    override def apply(_1: Term, _2: Term): Term = apply(Iterable(_1, _2))
//
//    implicit def asCollection(t: Term): C = t match {
//      case `unit` => newBuilder().result()
//      case t: NeCollectionNode if t.label == this => t.collection
//      case _ => (newBuilder() += t).result()
//    }
//
//    val unit = AssocModule.this.unit
//    val name: String = opLabel
//  }
//
//  case class NeCollectionNode(collection: C) extends CollectionNode[C] with Node2 {
//    val label = op
//    override lazy val _1: Term = collection.head
//    override lazy val _2: Term = ???
//
//    // label(collection.tail)
//    override def map(f: (Term) => Term): Term = label(collection.iterator.map(f).toIterable)
//  }
//
//  def matchContents(ksL: Seq[Term], ksR: Seq[Term]): Term = {
//    val res = (ksL, ksR) match {
//      case (Seq(), Seq()) => True
//      case (headL +: tailL, headR +: tailR) if headL == tailL => matchContents(tailL, tailR)
//      case (headL +: tailL, ksR) =>
//        (0 to ksR.size)
//          .map { index => (ksR.take(index), ksR.drop(index)) }
//          .map {
//            case (List(oneElement), suffix) =>
//              And(headL.unify(oneElement), matchContents(tailL, suffix))
//            case (prefix, suffix) =>
//              And(headL.unify(op(prefix)), matchContents(tailL, suffix))
//          }
//          .fold(False)({ (a, b) => Or(a, b) })
//
//      case other => False
//    }
//    res
//  }
//}
//
//class SET(val opLabel: String, val unit: Term) extends AssocModule {
//  type C = Set[Term]
//
//  override def newBuilder = Set.newBuilder[Term]
//
//  override def unify(a: Term, b: Term): Term = ???
//}
//
//class ASSOC_LIST(val opLabel: String, val unit: Term) extends AssocModule {
//  def this(opLabel: String) = this(opLabel, new Node0Label with InModule {
//    val name: String = "." + opLabel
//  }.apply())
//
//  type C = List[Term]
//
//  override def newBuilder() = List.newBuilder[Term]
//
//  def unify(a: Term, b: Term): Term = if (a.label == op) {
//    matchContents(op.asCollection(a), op.asCollection(b))
//  } else {
//    False
//  }
//}
//
//class MAP(val opLabel: String, val pairLabel: String, val unit: Term, falseOnDuplicate: Boolean = false) extends
//  AssocModule {
//  type C = MapOfPairs
//
//  object Pair extends Node2Label with InModule {
//    override val name = pairLabel
//
//    override def apply(_1: Term, _2: Term): Term = Node2(this, _1, _2)
//  }
//
//  case class MapOfPairs(m: Map[Term, Term]) extends IterableLike[Term, MapOfPairs] {
//    override def iterator: Iterator[Term] = m.map({ case (a, b) => Pair(a, b) }).iterator
//
//    override protected[this] def newBuilder = List.newBuilder[Term].mapResult(l =>
//      MapOfPairs(l map { case Pair(a, b) => (a, b) } toMap))
//
//    override def seq: TraversableOnce[Term] = m map { case (x, y) => Pair(x, y) }
//  }
//
//  override def newBuilder() = {
//    val b = Map.newBuilder[Term, Term]
//    new mutable.Builder[Term, MapOfPairs] {
//      override def +=(elem: Term): this.type = elem match {
//        case t: Node2 if t.label == Pair => b.+=((t._1, t._2)); this
//        case _ => throw new Exception("Expected a Pair but got something else: " + elem)
//      }
//
//      override def clear(): Unit = b.clear()
//
//      override def result(): MapOfPairs = MapOfPairs(b.result())
//    }
//  }
//
//  override def unify(a: Term, b: Term): Term = {
//    val aC: MapOfPairs = op.asCollection(a)
//    val bC: MapOfPairs = op.asCollection(b)
//    if (!falseOnDuplicate || (aC.m.keySet.intersect(bC.m.keySet).isEmpty)) {
//      op((aC.m ++ bC.m) map { case (a, b) => Pair(a, b) })
//    }
//    else {
//      False
//    }
//  }
//}
//
//object StructuralMatch {
//  def apply(a: Term, b: Term) = (a, b) match {
//    case (Token(aL, aV), Token(bL, bV)) => Truth(aL == bL && aV == bV)
//    case _ if a.label == b.label => a.iterator().zip(b.iterator()).map(p => p._1.unify(p._2)).reduce(And)
//    case _ => False
//  }
//}
//
//
//object KSEQ extends ASSOC_LIST("~>")
//
//object ID extends Module {
//
//  object Id extends LeafLabel[String] with InModule with NameFromObject
//
//  case class Id(value: String) extends Leaf[String] {
//    override val label: LeafLabel[String] = Id
//  }
//
//  override def unify(a: Term, b: Term): Term = ???
//}
//
//
///////// helpers
//
//object Node {
//  def apply(label: Node0Label) = FreeNode0(label)
//
//  def apply(label: Node1Label, _1: Term) = FreeNode1(label, _1)
//
//  def apply(label: Node2Label, _1: Term, _2: Term) = FreeNode2(label, _1, _2)
//
//  def apply(label: Node3Label, _1: Term, _2: Term, _3: Term) = FreeNode3(label, _1, _2, _3)
//}
