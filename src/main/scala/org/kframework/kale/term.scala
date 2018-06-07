package org.kframework.kale

import cats.Show
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.kframework.kale.highcats._
import org.kframework.kale.util.HasAtt
import org.roaringbitmap.RoaringBitmap

trait Label extends MemoizedHashCode with RoaringLabel {
  val env: Environment

  val name: String

  val id: Int = env.register(this)

  /**
    * None means that it depends on its children
    */
  val isPredicate: Option[Boolean]

  override def equals(other: Any): Boolean = other match {
    case that: Label => this.id == that.id
    case _ => false
  }

  override def computeHashCode: Int = id.hashCode

  override def toString: String = name
}

trait Term extends HasAtt with MemoizedHashCode with RoaringTerm {
  def updateAt(i: Int)(t: Term): Term

  val label: Label

  def isGround: Boolean

  lazy val size: Long = (children map (_.size)).sum + 1

  lazy val isPredicate: Boolean = label.isPredicate match {
    case Some(isPred) => isPred
    case None =>
      throw new AssertionError("Could not determine whether term is a predicate based on the label (" +
        label + ", which is hooked to " + label.getClass + "). Override isPredicate with correct specification.")
  }

  lazy val sort: Sort = label.env.sort(label, this.children.toSeq)

  def children: Iterable[Term]

  def map0(f: Term => Term): Term

  def canEqual(that: Any): Boolean = that match {
    case t: Term => t.label == this.label
    case _ => false
  }

  val variables: Set[Variable]

  /**
    * This method is called after `oldTerm` is updated resulting in `this` term.
    * Subclasses can override the method to attach functionality related to updating, e.g., updating attributes.
    * Should return `this`.
    */
  //  override def updatePostProcess(oldTerm: Term): Term = this


  def copy(children: Seq[Term]): Term

  override final def computeHashCode: Int = this.label.hashCode + 17 * this.children.hashCode
}

trait Predicate extends NotRoaring {
  self: Label =>

  val isPredicate = Some(true)
}

object Term {

  implicit val show = new Show[Term] {
    override def show(t: Term) = t.toConstructor
  }

  implicit def termDecoder(implicit env: Environment): Decoder[Term] = {
    Decoder.instance { (h: HCursor) =>
      val label = env.label(h.get[String]("label").right.get)

      label match {
        case leafLabel: LeafLabel[_] =>
          val data = h.get[String]("data").right.get
          val res = leafLabel.interpret(data)
          Right(res)
        case nodeLabel: NodeLabel =>
          val decoder = Decoder.decodeList[Term]
          decoder(h.downField("children").success.get).map({ children =>
            val res = nodeLabel(children)
            res
          })
      }
    }
  }

  implicit val termEncoder: Encoder[Term] = {
    Encoder.instance[Term] { t =>
      val labelAndAtts = Map("label" -> t.label.toString.asJson)
      t.label match {
        case label: LeafLabel[_] =>
          val label(data) = t
          (labelAndAtts + ("data" -> data.toString.asJson)).asJson
        case label: NodeLabel =>
          val node = t.asInstanceOf[Node]
          (labelAndAtts + ("children" -> node.children.asJson)).asJson
      }
    }
  }

  implicit class RichTerm(val t: Term) extends AnyVal {
    def moveRewriteToTop(implicit env: Environment): Rewrite = moveRewriteSymbolToTop(t)
    def down[O](implicit down: Down[O]): Option[O] = down.down(t)
  }

}

trait LeafLabel[T] extends (T => Leaf[T]) with Label {
  def unapply(t: Term): Option[T] = t match {
    case t: Leaf[T] if t.label == this => Some(t.data)
    case _ => None
  }

  // for KORE
  def interpret(str: String): Term = this (internalInterpret(str))

  protected[this] def internalInterpret(s: String): T
}

trait Leaf[T] extends Term {
  def children: Iterable[Term] = Iterable.empty

  def map0(f: Term => Term): Term = this

  def updateAt(i: Int)(t: Term): Term = throw new IndexOutOfBoundsException("Leaves have no children. Trying to update index _" + i)

  val label: LeafLabel[T]
  val data: T

  override def toString: String = label + "(" + data.toString + ")"

  def copy(): Term = label(data).updatePostProcess(this)

  def copy(children: Seq[Term]): Term = {
    assert(children.isEmpty)
    copy()
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Leaf[_] => that.label == this.label && this.hashCode == that.hashCode && that.data == this.data
    case _ => false
  }

  override val variables: Set[Variable] = Set()
}

trait NodeLabel extends Label {
  def unapplySeq(t: Term): Option[Seq[Term]] = t match {
    case t: Node if t.label == this => Some(t.children.toSeq)
    case _ => None
  }

  val arity: Int

  def apply(l: Iterable[Term]): Term = if (l.size == arity) {
    constructFromChildren(l)
  } else {
    throw new AssertionError("Incorrect number of children for constructing a " + name + ". Expected: " + arity + " but found: " + l.size)
  }

  protected def constructFromChildren(l: Iterable[Term]): Term
}

trait Node extends Term with Product {
  val label: NodeLabel

  def updateAt(i: Int)(t: Term): Term = if (i < 0 || i >= productArity) {
    throw new IndexOutOfBoundsException(label + " has " + productArity + " children. Trying to update index _" + i)
  } else {
    innerUpdateAt(i, t)
  }

  protected def innerUpdateAt(i: Int, t: Term): Term

  def children: Iterable[Term]

  override def toString: String = label + "(" + children.mkString(", ") + ")"

  def copy(children: Seq[Term]): Term

  override lazy val variables: Set[Variable] = children.flatMap(_.variables).toSet

  override def equals(obj: Any): Boolean = obj match {
    case n: Node => n.label == this.label && n.hashCode == this.hashCode && n.children == this.children
    case _ => false
  }
}

object Node {
  def unapply(t: Term): Option[(NodeLabel, Iterable[Term])] = t match {
    case t: Node => Some(t.label, t.children)
    case _ => None
  }
}

object Leaf {
  def unapply(t: Term): Option[(LeafLabel[T], T) forSome {type T}] = t match {
    case l: Leaf[_] => Some(l.label, l.data)
    case _ => None
  }
}
