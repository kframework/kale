package org.kframework.kale

import org.roaringbitmap.{FastAggregation, RoaringBitmap}

trait RoaringOptimization {
  self: Label =>

  import collection.JavaConverters._

  def requiredLabels(children: Iterable[Term]): RoaringBitmap

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap

  @inline protected def requiredFor(children: Iterable[Term]) = {
    RoaringBitmap.or((children map (_.requiredLabels) toIterator).asJava)
  }

  @inline protected def suppliedBy(children: Iterable[Term]) = {
    RoaringBitmap.or((children map (_.suppliedLabels) toIterator).asJava)
  }

  @inline protected def addThis(r: RoaringBitmap): RoaringBitmap = {
    r.add(this.id)
    r
  }
}

trait NotRoaring {
  self: Label with RoaringOptimization =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap =
    throw new AssertionError("Should not look for required labels in predicates")

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap =
    throw new AssertionError("Should not look for supplied labels in predicates")
}

trait CluelessRoaring {
  self: Label with RoaringOptimization =>

  override def requiredLabels(children: Iterable[Term]) = RoaringBitmap.bitmapOf()

  override def suppliedLabels(children: Iterable[Term]) = env.allLabelIds
}

trait ThisRoaring {
  self: Label with RoaringOptimization =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap = RoaringBitmap.bitmapOf(this.id)

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap = RoaringBitmap.bitmapOf(this.id)
}

trait ConjunctiveRoaring {
  self: Label with RoaringOptimization =>

  def requiredLabels(children: Iterable[Term]): RoaringBitmap = addThis(requiredFor(children))

  def suppliedLabels(children: Iterable[Term]): RoaringBitmap = addThis(suppliedBy(children))
}

trait DisjunctiveRoaring {
  self: Label with RoaringOptimization =>

  override def requiredLabels(children: Iterable[Term]) = FastAggregation.and(children map (_.requiredLabels) toSeq: _*)

  override def suppliedLabels(children: Iterable[Term]) = suppliedBy(children)
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