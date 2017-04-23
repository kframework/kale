package org.kframework.kale.util

import org.kframework.kale.Term

trait Att[T] {
  def default(): T

  def update(oldValue: T, term: Term, oldChildren: Option[Iterable[Term]]): T
}

object MutableObj {
  def apply[T](v: T) = new MutableObj[T](v)
}

final class MutableObj[T](private var v: T) extends Mutable {
  def set(v: T): Unit = {
    this.v = v
  }

  def value(): T = v

  override def toString: String = "MutableObject(" + v + ")"

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: MutableObj[_] => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = value.hashCode()
}

trait HasAtt {
  self: Term =>

  //  protected[this]
  var attributes: Map[Att[_], _] = Map()

  def updateAttributes(oldChildren: Option[Iterable[Term]]): Unit = {
    attributes = (this.attributes map {
      case (k, v) => (k, k.asInstanceOf[Att[Any]].update(v, this, oldChildren))
    }).toMap
  }

  def att[T](att: Att[T]): T = {
    if (!attributes.contains(att)) {
      val newValue = att.update(att.default(), this, None)
      attributes = attributes + (att -> newValue)
    }
    attributes(att).asInstanceOf[T]
  }

  def updatePostProcess(oldTerm: Term): Term = {
    val oldChildren =
      if (oldTerm.label == this.label)
        Some(oldTerm.children)
      else
        None

    this.updateAttributes(oldChildren)
    this
  }
}