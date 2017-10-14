package org.kframework.kale.highcats

import cats._
import cats.implicits._
import cats.{Apply, Eq}
import org.kframework.kale.util.LabelNamed
import org.kframework.kale.{Environment, FreeLabel1, FreeLabel2, FreeLabel3, Term}

trait Free {
  _: Environment =>

  abstract class ObjectNamed[R](companionObject: R) extends LabelNamed(companionObject.getClass.getSimpleName.split("\\$").head) {
  }

  def free1[A: UpDown, R <: Product](companionObject: A => R) =
    new ObjectNamed(companionObject) with FreeLabel1 with UpDown[R] {
      private def split(o: R): A = {
        o.productIterator.next().asInstanceOf[A]
      }
      override def down(t: Term): Option[R] = unapply(t) flatMap {
        case (a) => a.down[A] map companionObject
      }
      override def up(o: R): Term = apply(split(o))
    }

  def free2[A: UpDown, B: UpDown, R <: Product](companionObject: (A, B) => R) =
    new ObjectNamed(companionObject) with FreeLabel2 with UpDown[R] {
      private def split(o: R): (A, B) = {
        val x = o.productIterator
        (x.next(), x.next()).asInstanceOf[(A, B)]
      }
      override def down(t: Term) = unapply(t) flatMap {
        case (a, b) => Apply[Option].map2(a.down[A], b.down[B])(companionObject)
      }
      override def up(o: R) = split(o) match {
        case (a, b) => apply(a, b)
      }
    }

  def free3[A: UpDown, B: UpDown, C: UpDown, R <: Product](companionObject: (A, B, C) => R) =
    new ObjectNamed(companionObject) with FreeLabel3 with UpDown[R] {
      private def split(o: R): (A, B, C) = {
        val x = o.productIterator
        (x.next(), x.next(), x.next()).asInstanceOf[(A, B, C)]
      }
      override def down(t: Term) = unapply(t) flatMap {
        case (a, b, c) => Apply[Option].map3(a.down[A], b.down[B], c.down[C])(companionObject)
      }
      override def up(o: R): Term = {
        split(o) match {
          case (a, b, c) => apply(a, b, c)
        }
      }
    }

}
