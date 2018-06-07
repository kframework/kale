package org.kframework.kale

import org.roaringbitmap.{FastAggregation, RoaringBitmap}

object Roaring {

  import collection.JavaConverters._

  @inline def requiredFor(children: Iterable[Term]) = {
    RoaringBitmap.or((children map (_.requiredLabels) toIterator).asJava)
  }

  @inline def suppliedBy(children: Iterable[Term]) = {
    RoaringBitmap.or((children map (_.suppliedLabels) toIterator).asJava)
  }
}

trait RoaringLabel {
  self: Label =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap

  @inline protected def addThis(r: RoaringBitmap): RoaringBitmap = {
    r.add(this.id)
    r
  }
}

trait RoaringTerm {
  self: Term =>

  lazy val requiredLabels: RoaringBitmap = label.requiredLabels(children)

  lazy val suppliedLabels: RoaringBitmap = label.suppliedLabels(children)
}

trait NotRoaringTerm {
  _: Term with RoaringTerm =>

  override lazy val requiredLabels: RoaringBitmap =
    throw new AssertionError("Should not look for required labels in predicates")

  override lazy val suppliedLabels: RoaringBitmap =
    throw new AssertionError("Should not look for supplied labels in predicates")
}

trait CluelessRoaringTerm {
  _: Term with RoaringTerm =>

  override lazy val requiredLabels: RoaringBitmap = RoaringBitmap.bitmapOf()

  override lazy val suppliedLabels: RoaringBitmap = label.env.allLabelIds
}

trait NotRoaring {
  self: Label with RoaringLabel =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap =
    throw new AssertionError("Should not look for required labels in predicates")

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap =
    throw new AssertionError("Should not look for supplied labels in predicates")
}

trait CluelessRoaring {
  self: Label with RoaringLabel =>

  override def requiredLabels(children: Iterable[Term]) = RoaringBitmap.bitmapOf()

  override def suppliedLabels(children: Iterable[Term]) = env.allLabelIds
}

trait ThisRoaring {
  self: Label with RoaringLabel =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap = RoaringBitmap.bitmapOf(this.id)

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap = RoaringBitmap.bitmapOf(this.id)
}

/**
  * Requires and supplies all children
  */
trait ConjunctiveRoaring {
  self: Label with RoaringLabel =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap = addThis(Roaring.requiredFor(children))

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap = addThis(Roaring.suppliedBy(children))
}

trait DisjunctiveRoaring {
  self: Label with RoaringLabel =>

  override def requiredLabels(children: Iterable[Term]) = FastAggregation.and(children map (_.requiredLabels) toSeq: _*)

  override def suppliedLabels(children: Iterable[Term]) = Roaring.suppliedBy(children)
}

trait Projection1Roaring {
  self: Label =>

  override def requiredLabels(children: Iterable[Term]) = children.head.requiredLabels

  override def suppliedLabels(children: Iterable[Term]) = children.head.suppliedLabels
}

trait Projection2Roaring {
  self: Label =>

  override def requiredLabels(children: Iterable[Term]) = children.tail.head.requiredLabels

  override def suppliedLabels(children: Iterable[Term]) = children.tail.head.suppliedLabels
}

trait RoaringMixin {
  _: Environment =>

  lazy val allLabelIds: RoaringBitmap = RoaringBitmap.bitmapOf(uniqueLabels map (_._2.id) toSeq: _*)
}