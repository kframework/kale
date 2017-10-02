package org.kframework.kale

import cats.Foldable
import org.kframework.kale.standard.{ReferenceLabel, ScalaLibraryMixin}

trait Convert[A, B] {
  def convert(a: A): B
}

trait Up[O] extends (O => Term)

trait Down[O] {
  def unapply(t: Term): Option[O]
}

trait UpDown[O] extends Up[O] with Down[O]

trait MonoidLabeled[O[_]] {
  def monoidLabel: MonoidLabel
}

trait DefineMixin extends Mixin {
  _: Environment =>

  def define[A: UpDown, B: UpDown, R: UpDown](name: String, f: (A, B) => R): PrimitiveFunction2[A, B, R] =
    PrimitiveFunction2(name, implicitly[UpDown[A]], implicitly[UpDown[B]], implicitly[UpDown[R]], f)

  def define[A: UpDown, B: UpDown, C: UpDown, R: UpDown](name: String, f: (A, B, C) => R): PrimitiveFunction3[A, B, C, R] =
    PrimitiveFunction3(name, implicitly[UpDown[A]], implicitly[UpDown[B]], implicitly[UpDown[C]], implicitly[UpDown[R]], f)

  def define[A: UpDown, R: UpDown](name: String, f: A => R): PrimitiveFunction1[A, R] =
    PrimitiveFunction1(name, implicitly[UpDown[A]], implicitly[UpDown[R]], f)

  def define[T](name: String)(implicit ConvertFromString: Convert[String, T]): ReferenceLabel[T] = new ReferenceLabel[T](name) {
    override protected[this] def internalInterpret(s: String): T = ConvertFromString.convert(s)
  }
}

class ScalaListUp[O: Up](implicit env: Environment with ScalaLibraryMixin) extends Up[List[O]] {
  override def apply(l: List[O]) = env.scalaList(l map implicitly[Up[O]])
}

object Up {
  final def apply[O](implicit instance: Up[O]): Up[O] = instance

  final def apply[O](f: O => Term): Up[O] = new Up[O] {
    override def apply(o: O): Term = f(o)
  }

  implicit def upMonoidLabeled[O[_] : MonoidLabeled : Foldable, E: Up]: Up[O[E]] = new Up[O[E]] {
    override def apply(o: O[E]) = {
      implicitly[Foldable[O]].foldMap(o)(implicitly[Up[E]])(implicitly[MonoidLabeled[O]].monoidLabel)
    }
  }
}
