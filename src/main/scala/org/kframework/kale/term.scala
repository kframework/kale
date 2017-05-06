package org.kframework.kale

import io.circe.{Decoder, Encoder, HCursor}
import org.kframework.kale.util.{HasAtt, Util}
import org.kframework.kore
import org.kframework.kore.implementation.DefaultBuilders
import io.circe.syntax._

trait Label extends MemoizedHashCode with kore.Symbol {
  val env: Environment

  val name: String

  def smt: String = name

  val id: Int = env.register(this)

  override def equals(other: Any): Boolean = other match {
    case that: Label => this.name == that.name
    case _ => false
  }

  override def computeHashCode: Int = name.hashCode

  override def toString: String = name

  // FOR KORE
  override val str: String = name
}

trait Term extends kore.Pattern with HasAtt {
  def updateAt(i: Int)(t: Term): Term

  val label: Label

  val isGround: Boolean

  val isPredicate: Boolean

  lazy val sort: Sort = label.env.sort(label, this.children.toSeq)

  def children: Iterable[Term]

  def canEqual(that: Any): Boolean = that match {
    case t: Term => t.label == this.label
    case _ => false
  }

  /**
    * This method is called after `oldTerm` is updated resulting in `this` term.
    * Subclasses can override the method to attach functionality related to updating, e.g., updating attributes.
    * Should return `this`.
    */
//  override def updatePostProcess(oldTerm: Term): Term = this

  // TODO: should experiment with other implementations
  override def hashCode: Int = this.label.hashCode

  def copy(children: Seq[Term]): Term
}

object Term {
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

  implicit class StaticRichTerm(t: Term) {
    def contains(subterm: Term): Boolean = Util.contains(t, subterm) // if (t == subterm) true else t.children.exists(_.contains(subterm))
    def containsInConstructor(subterm: Term): Boolean = Util.containsInConstructor(t, subterm)
  }

  implicit class RichTerm(t: Term)(implicit env: Environment) {
    def moveRewriteToTop: Rewrite = Util.moveRewriteSymbolToTop(t)
  }
}

trait LeafLabel[T] extends Label {
  def apply(t: T): Term

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
    case that: Leaf[_] => that.label == this.label && that.data == this.data
    case _ => false
  }
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
}

object Node {
  def unapply(t: Term): Option[(NodeLabel, Iterable[Term])] = t match {
    case t: Node => Some(t.label, t.children)
    case _ => None
  }
}
