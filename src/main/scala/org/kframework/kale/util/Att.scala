package org.kframework.kale.util

import org.kframework.kale.Term

import scala.collection._

trait Key[T] {
  def name: String = this.getClass.getName
}

trait Att[T] {
  def value: T

  def key: Class[_ <: Att[T]] = this.getClass.asInstanceOf[Class[Att[T]]]

  def update(term: Term with HasAtt): Att[T]

  def update(term: Term with HasAtt, oldChildren: Iterable[Term with HasAtt]): Att[T]
}

case class SimpleAtt[T](value: T) extends Att[T] {
  override def update(term: Term with HasAtt): Att[T] = this

  override def update(term: Term with HasAtt, oldChildren: Iterable[Term with HasAtt]): Att[T] = this
}

trait HasAtt {
  self: Term =>

  //  protected[this]
  var attributes = Map[Class[_], Att[_]]()

  def updatedAttributes(newTerms: Term with HasAtt*): Map[Class[_], Att[_]] = (this.attributes map {
    case (k, v) => (k, v.update(this, newTerms))
  }).toMap

  def setAtt(value: Att[_]): Term = {
    attributes = attributes + (value.getClass -> value)
    this
  }

  def getAtt[T <: Att[_]](key: Class[T]): T = attributes(key).asInstanceOf[T]

  def getAttOption[T <: Att[_]](key: Class[T]): Option[T] = attributes.get(key).asInstanceOf[Option[T]]

  def hasAtt[T <: Att[_]](key: Class[T]): Boolean = attributes.contains(key)

  def setAtts(atts: Map[Class[_], Att[_]]): Term = {
    attributes = atts
    this
  }

  def updatePostProcess(oldTerm: Term): Term = {
    this.setAtts(updatedAttributes(oldTerm.children.toSeq.asInstanceOf[Seq[Term with HasAtt]]: _*))
  }
}