package org.kframework.kale.util

import org.kframework.kale.Term

trait Att[T] {
  def update(term: Term, oldTermOption: Option[Term]): T
}

trait HasAtt {
  self: Term =>

  //  protected[this]
  var attributes: Map[Att[_], _] = Map()

  def updateAttributes(oldTerm: Term): Unit = {
    attributes = (oldTerm.attributes map {
      case (k, _) => (k, k.asInstanceOf[Att[Any]].update(this, Some(oldTerm)))
    }).toMap
  }

  def set[T](att: Att[T], value: T): Unit = attributes += (att -> value)

  def hasAtt(att: Att[_]): Boolean = attributes.contains(att)

  def att[T](att: Att[T]): T = {
    if (!attributes.contains(att)) {
      val newValue = att.update(this, None)
      attributes = attributes + (att -> newValue)
    }
    attributes(att).asInstanceOf[T]
  }

  def updatePostProcess(oldTerm: Term): Term = {
    this.updateAttributes(oldTerm)
    this
  }
}