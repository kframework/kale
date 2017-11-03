package org.kframework.kale.highcats

import org.kframework.kale.{Label, LeafLabel, Term}

trait Up[O] {
  def up(o: O): Term
  def label: Label
}

trait Down[O] {
  def down(t: Term): Option[O]

  object extract {
    def unapply(t: Term): Option[O] = down(t)
  }
}

object UpDown {
  final def apply[O](implicit instance: Up[O]): Up[O] = instance

  final def apply[O](f: O => Term, l: LeafLabel[O]): Up[O] = new Up[O] {
    override def up(o: O) = f(o)
    override def label: LeafLabel[O] = l
  }

  final implicit def updownFromLeafLabel[O](implicit l: LeafLabel[O]): UpDown[O] = new UpDown[O] {
    override def down(t: Term) = l.unapply(t)

    override def up(o: O) = l.apply(o)

    override def label: LeafLabel[O] = l
  }
}

trait UpDown[O] extends Up[O] with Down[O]